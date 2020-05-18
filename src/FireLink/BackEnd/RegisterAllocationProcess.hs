{-|
Module      : FireLink.BackEnd.RegisterAllocationProcess
Description : Register allocation algorithm by graph coloration
Stability   : experimental

Attempts to allocate registers by doing chaitan algorithm as described here:
https://cs.gmu.edu/~white/CS640/p98-chaitin.pdf

[@initialStep@] process all codeblocks as we have infinite registers. Also, on function calls,
all live variables at that moment are going to be "stored" before them. That is done to
avoid that the interference graph will have edges between their own variables.

Register allocation is made using optimistic colouring algorithm from Chaitan/Briggs

Data.Graph.Graph represents directed graphs, but we actually need undirected. So, we need
to remember that the existence of edge (i, j) implies that (j, i) exists (when i /= j)

-}
module FireLink.BackEnd.RegisterAllocationProcess (run, RegisterAssignment, Register(..)) where

import qualified Control.Monad.State                 as State
import qualified Data.Array                          as A
import           Data.Char                           (isDigit)
import qualified Data.Graph                          as G
import qualified Data.Map                            as Map
import           Data.Map.Internal.Debug             (showTree)
import qualified Data.Set                            as Set
import           FireLink.BackEnd.CodeGenerator      (TAC (..),
                                                      TACSymEntry (..))
import           FireLink.BackEnd.FlowGraphGenerator (BasicBlock,
                                                      FlowGraph (..),
                                                      NumberedBlock,
                                                      getProgramVariables)
import           FireLink.BackEnd.LivenessAnalyser
import           FireLink.BackEnd.Utils
import           FireLink.FrontEnd.SymTable          (Dictionary (..),
                                                      findArgsByFunName,
                                                      getOffset)
import qualified FireLink.FrontEnd.SymTable          as ST (DictionaryEntry (..))
import           TACType

-- | To avoid confusions between G.Vertex and registers
-- | In order to allow registers for floating point
newtype Register = Register String
    deriving (Eq, Ord)

instance Show Register where
    show (Register r) = "$" ++ r

type Color = Register

availableColors :: Set.Set Color
availableColors = Set.map Register availableRegisters

-- | Associate a variable with a color (register). If the variable isn't in the
-- | map we can say that it needs to be spilled
type RegisterAssignment = Map.Map TACSymEntry Register


-- | Coloration state for the optimistic coloring algorithm by Chaitan/Briggs, as exposed
-- | on classroom slides.
data ColorationState = ColorationState

    -- | Used only by @simplify@ function
    { csGraph :: !G.Graph -- ^ state graph
    , csDeletedVertices :: !(Set.Set G.Vertex) -- ^ Set of deleted vertices
    , csVertexStack :: ![G.Vertex] -- ^ vertex stack to recolor in `select` phase
    , csSpilledVertices :: !(Set.Set G.Vertex) -- ^ Set of spilled vertices

    -- | Used in `select` phase
    , csColoredVertices :: !(Map.Map G.Vertex Color) -- ^ Map of vertex to color, where vertex represents a virtual register

    -- | General use properties
    , csProgramVariables :: !(Set.Set TACSymEntry) -- ^ All program variables, never updated
    , csProgramFlowGraph :: !FlowGraph -- ^ Whole program flow-graph = (NumberedBlocks, Graph)
    , csInterferenceGraph :: !InterferenceGraph -- ^ Current interference graph
    , csLivenessInformation :: ![LineLiveVariables] -- ^ in/out liveness information for each line
    , csSpillsCostMap :: !(Map.Map TACSymEntry Int) -- ^ What is the cost of spilling a variable?
    }

-- | Data.Graph graph representation is made by a range of integers describeing the set of vertices
-- | So we can't exactly delete a single vertex, only remove all edges where it appears.
-- | Even Data.Graph.outdegree will say that its outdegree is 0, so we need to have a set of deleted vertices
-- | as well.
type ColorState = State.StateT ColorationState IO

-------------------------------------------------------
----- Helper functions to operate with ColorState -----
-------------------------------------------------------

-- | Helper to get the colored vertices
getColoredVerticesMap :: ColorState (Map.Map G.Vertex Color)
getColoredVerticesMap = csColoredVertices <$> State.get

-- | Helper to modify colored vertices map state
putColoredVertices :: Map.Map G.Vertex Color -> ColorState ()
putColoredVertices m = do
    c <- State.get
    State.put c{csColoredVertices = m}

-- | Helper to color a vertex
colorVertex :: G.Vertex -> Color -> ColorState ()
colorVertex vertex color = do
    m <- getColoredVerticesMap
    putColoredVertices $ Map.insert vertex color m

-- | Helper to get spilled vertices
getSpilledVertices :: ColorState (Set.Set G.Vertex)
getSpilledVertices = csSpilledVertices <$> State.get

-- | Helper to put spilled vertices
putSpilledVertices :: Set.Set G.Vertex -> ColorState ()
putSpilledVertices v = do
    c <- State.get
    State.put c{csSpilledVertices = v}

-- | Add vertex to the set of spilled vertices
addVertexToSpilledVertices :: G.Vertex -> ColorState ()
addVertexToSpilledVertices vertex = do
    spilledVertices <- getSpilledVertices
    putSpilledVertices $ Set.insert vertex spilledVertices

getAssocVariable :: G.Vertex -> ColorState TACSymEntry
getAssocVariable vertex = do
    (vertexToVarMap, _) <- getInterferenceGraph
    return $ vertexToVarMap Map.! vertex

-- | Helper to get program variables
getProgramVariables' :: ColorState (Set.Set TACSymEntry)
getProgramVariables' = csProgramVariables <$> State.get

-- | Helper to get spills cost map
getSpillsCostMap :: ColorState (Map.Map TACSymEntry Int)
getSpillsCostMap = csSpillsCostMap <$> State.get

-- | Associate cost i with var v
associateSpillCost :: TACSymEntry -> Int -> ColorState ()
associateSpillCost v i = do
    spillMap <- getSpillsCostMap
    c <- State.get
    State.put c{csSpillsCostMap = Map.insert v i spillMap}

-- | Get cost for var v, defaults to 0
getVarSpillCost :: TACSymEntry -> ColorState Int
getVarSpillCost v = do
    spillMap <- getSpillsCostMap
    return $ Map.findWithDefault 0 v spillMap

-- | Helper to get whole program from state
getProgramFlowGraph :: ColorState FlowGraph
getProgramFlowGraph = csProgramFlowGraph <$> State.get

-- | Helper to replace program flow graph
putProgramFlowGraph :: FlowGraph -> ColorState ()
putProgramFlowGraph flowGraph = do
    c <- State.get
    State.put c{csProgramFlowGraph = flowGraph}

-- | Helper to replace interference graph on current state
putInterferenceGraph :: InterferenceGraph -> ColorState ()
putInterferenceGraph interferenceGraph = do
    c <- State.get
    State.put c{csInterferenceGraph = interferenceGraph}

-- | Helper to get interference grpah on current state
getInterferenceGraph :: ColorState InterferenceGraph
getInterferenceGraph = csInterferenceGraph <$> State.get

-- | helper to replace livenesss information
putLivenessInformation :: [LineLiveVariables] -> ColorState ()
putLivenessInformation ls = do
    c <- State.get
    State.put c{csLivenessInformation = ls}

-- | Helper to get the graph in a ColorState execution
getGraph :: ColorState G.Graph
getGraph = csGraph <$> State.get

-- | Helper to replace the graph in a ColorState execution
putGraph :: G.Graph -> ColorState ()
putGraph graph = do
    c <- State.get
    State.put c{csGraph = graph}

-- | Helper to get deleted vertices set in a ColorState execution
getDeletedVertices :: ColorState (Set.Set G.Vertex)
getDeletedVertices = csDeletedVertices <$> State.get

putDeletedVertices :: Set.Set G.Vertex -> ColorState ()
putDeletedVertices vertices = do
    c <- State.get
    State.put c{csDeletedVertices = vertices}

-- | Helper to get current stack
getVertexStack :: ColorState [G.Vertex]
getVertexStack = csVertexStack <$> State.get

-- | Helper to put register stack
putRegisterStack :: [G.Vertex] -> ColorState ()
putRegisterStack stack = do
    c <- State.get
    State.put c{csVertexStack = stack}

-- | Push a register on top of the state's stack
pushVertex :: G.Vertex -> ColorState ()
pushVertex reg = do
    stack <- getVertexStack
    putRegisterStack $ reg : stack


graphBounds :: G.Graph -> G.Bounds
graphBounds graph = (0, length (G.vertices graph) - 1)

deleteVertex :: G.Vertex -> ColorState ()
deleteVertex vertex = do
    graph <- getGraph
    deletedVertices <- getDeletedVertices
    let newEdges = filter (\(a, b) -> a /= vertex && b /= vertex) $ G.edges graph

    putGraph $ G.buildG (graphBounds graph) newEdges
    putDeletedVertices $ vertex `Set.insert` deletedVertices

----------------------------------
-------- Actual algorithm --------
----------------------------------

-- | Generate attempting final TAC assumming that we will have infinite registers i.e one register per actual variable
-- | Also, before and after functions call live variables at that point will be saved to and loaded from memory respectively.
-- | This is done to reduce the size of the interference graph, and because variables from different functions are not supposed
-- | to interfere between them.
initialStep :: FlowGraph -> Dictionary -> FlowGraph
initialStep flowGraph@(numberedBlocks, graph) dict = (preProcessCode, graph)
    where
        programVariables :: Set.Set TACSymEntry
        programVariables = getProgramVariables numberedBlocks

        initialLivenessAnalysis :: [LineLiveVariables]
        initialLivenessAnalysis = livenessAnalyser flowGraph

        getLivenessIn :: ProgramPoint -> LineLiveVariables
        getLivenessIn point = head $ filter ((point ==) . llvInstrId) initialLivenessAnalysis

        isTacLabelFun :: TAC -> Bool
        isTacLabelFun (ThreeAddressCode NewLabel _ (Just (Label (l : _))) _) =
            -- case for _main function, that isn't on symtable
            l /= '_' &&
            -- jump labels, functions do not begin with digits
            not (isDigit l)

        isTacLabelFun _ = False

        preProcessCode :: [NumberedBlock]
        preProcessCode = map go numberedBlocks

        go :: NumberedBlock -> NumberedBlock
        go (index, block) =
            (index, concatMap (go' . \(instrIdx, tac) -> ((index, instrIdx), tac)) $ zip [0..] block)

        getFunctionArguments :: String -> [TACSymEntry]
        getFunctionArguments funName =
            let dictEntryArguments = findArgsByFunName funName dict in
                map (\entry -> TACVariable entry (getOffset entry)) dictEntryArguments

        go' :: (ProgramPoint, TAC) -> [TAC]
        -- If we find a function call, we need to store all live-variables at that point
        go' (programPoint, tac@(ThreeAddressCode Call maybeVar _ _)) =
            let liveVariablesInOut = getLivenessIn programPoint
                storeTacs = map (\tacSymEntry ->
                    ThreeAddressCode Store (Just (Id tacSymEntry)) Nothing Nothing) $
                        Set.toList $ llvInLiveVariables liveVariablesInOut
                toDeleteAfterCall = case maybeVar of
                                        Nothing     -> Set.empty
                                        Just (Id v) -> Set.singleton v
                loadTacs = map (\tacSymEntry ->
                    ThreeAddressCode Load (Just (Id tacSymEntry)) Nothing Nothing) $
                        Set.toList $ llvOutLiveVariables liveVariablesInOut Set.\\ toDeleteAfterCall
                in storeTacs ++ [tac] ++ loadTacs

        -- after generating code for labels, we need to load the used-arguments to their registers.
        -- if a var is not in the variableRegisterMap then it is not used in the program i.e. we can
        -- skip its load
        go' (_, tac@(ThreeAddressCode NewLabel _ (Just (Label funName)) _)) =
            if isTacLabelFun tac then
                let args = getFunctionArguments funName
                    loadTacsSet = map (\tacSymEntry ->
                        [ThreeAddressCode
                            Load (Just (Id tacSymEntry)) Nothing Nothing | tacSymEntry `Set.member` programVariables]
                        ) args
                    loadTacs = concat loadTacsSet in
                        tac : loadTacs
            else [tac]
        go' (_, tac) = [tac]


-- | Do liveness analysis on a program to construct the interference graph.
-- state properties used:
-- * csProgramFlowGraph, read
-- * csInterferenceGraph, write
-- * csLivenessInformation, write
build :: ColorState ()
build = do
    flowGraph <- getProgramFlowGraph
    let (interferenceGraph, livenessInfo) = generateInterferenceGraph' flowGraph
    putInterferenceGraph interferenceGraph
    putLivenessInformation livenessInfo


-- | Estimate by each variable its cost of not having a spill for it, by using following heuristics
-- | - number of definitions + number of usages
-- | - TODO: try to implement cycle costs formula
-- |
-- | The final cost for each variable is the sum of all of this heuristics.
-- state properties used:
-- * csProgramVariables, read
-- * csProgramFlowGraph, read
-- * csSpillsCostMap, write
costs :: ColorState ()
costs = do
    programVars <- getProgramVariables'
    mapM_ applyHeuristics programVars
    where
        numberOfDefinitionsAndUsages :: TACSymEntry -> ColorState Int
        numberOfDefinitionsAndUsages var = do
            (numberedBlocks, _) <- getProgramFlowGraph
            let wholeProgram = concatMap snd numberedBlocks
            let d = sum $ map (defines var) wholeProgram
            let u = sum $ map (usages var) wholeProgram
            return $ d + u

        defines :: TACSymEntry -> TAC -> Int
        defines var tac = if var `Set.member` def1 tac then 1 else 0

        usages :: TACSymEntry -> TAC -> Int
        usages var tac = if var `Set.member` use1 tac then 1 else 0

        heuristics :: [TACSymEntry -> ColorState Int]
        heuristics = [numberOfDefinitionsAndUsages]

        applyHeuristics :: TACSymEntry -> ColorState ()
        applyHeuristics var = do
            costs <- mapM (\f -> f var) heuristics
            let sumOfCosts = sum costs
            associateSpillCost var sumOfCosts

-- | Attempts to color a graph with the number of general-purpose registers
-- state properties used:
-- * csInterferenceGraph, read
-- * csGraph, write
-- * csDeletedVertices, write
-- * csRegisterStack, write
-- * csSpilledVertices, write
-- * csSpillsCostMap, read
simplify :: ColorState ()
simplify = prepareState >> go
    where
        prepareState :: ColorState ()
        prepareState = do
            (_, interferenceGraph) <- getInterferenceGraph
            putGraph interferenceGraph
            putDeletedVertices Set.empty
            putRegisterStack []
            putSpilledVertices Set.empty

        numberOfVertices :: ColorState Int
        numberOfVertices = do
            (_, interferenceGraph) <- getInterferenceGraph
            return $ length $ G.vertices interferenceGraph

        numberOfColors :: Int
        numberOfColors = length availableRegisters

        -- Attempt to choose a vertex
        chooseVertexWithLessthanK :: ColorState (Maybe G.Vertex)
        chooseVertexWithLessthanK = do
            graph <- getGraph
            deletedVertices <- getDeletedVertices
            let vertices = G.vertices graph
            let graphOutDegreePerVertex = A.assocs $ G.outdegree graph
            -- We can choose a vertex which outdegree is between 0 and "numberOfColors", inclusive and exclusive.
            -- Also, since Data.Graph doesn't provide the deletion of vertices, we need to keep a set of _deleted_
            -- vertices
            case filter (\(vertex, outdegree) ->
                    0 <= outdegree && outdegree < numberOfColors &&
                    vertex `Set.notMember` deletedVertices) graphOutDegreePerVertex of
                []         -> return Nothing
                (a, _) : _ -> return $ Just a

        -- | Attempts to choose a vertex to spill based on the `costs` map
        -- | TODO: try to implement heuristic of choosing vertex that will make its neightbours have less than k
        -- | neighbours
        chooseVertexToSpill :: ColorState G.Vertex
        chooseVertexToSpill = do
            currGraph <- getGraph
            deletedVertices <- getDeletedVertices
            let currentVertices = Set.toList $ Set.fromList (G.vertices currGraph) Set.\\ deletedVertices
            costsWithVertex <- mapM lookupCost currentVertices
            let (spilledVertex, _) = foldl (\v@(_, vcost) a@(_, acost) -> if vcost > acost then v else a)
                                        (-1, -1) costsWithVertex
            return spilledVertex

        lookupCost :: G.Vertex -> ColorState (G.Vertex, Int)
        lookupCost vertex = (,) vertex <$> (getAssocVariable vertex >>= getVarSpillCost)

        -- Returns a stack of vertices that emulated the reverse order of vertices that were delete
        go :: ColorState ()
        go = do
            deletedVertices <- getDeletedVertices
            numOfVertices <- numberOfVertices
            -- | If every vertex was deleted, then coloration is finished
            if Set.size deletedVertices == numOfVertices then
                return ()
            -- | Otherwise, let's keep coloration
            else do
                maybeChoosenVertex <- chooseVertexWithLessthanK
                v <- case maybeChoosenVertex of
                    -- | Nice! we can retrieve this vertex and add it to the stack
                    Just v -> return v
                    -- | TODO: handle case when we can't get a vertex with less than k neighbours
                    Nothing -> do
                        v <- chooseVertexToSpill
                        addVertexToSpilledVertices v
                        return v
                deleteVertex v
                pushVertex v
                go

-- | Based on the stack, the actual interference graph and knowing that **no** register was spilled, we
-- | finally attempt to color registers
-- state properties used:
-- * csVertexStack, read
-- * csProgramFlowGraph, read
-- * csInterferenceGraph, read
-- * csColoredVertices, write
-- * csGraph, write
select :: ColorState (RegisterAssignment, FlowGraph)
select = do
    colorationStack <- getVertexStack
    mapM_ assignColor colorationStack
    flowGraph <- getProgramFlowGraph
    vertexColorMap <- getColoredVerticesMap
    (vertexToVarMap, _) <- getInterferenceGraph
    let mapVarToColor (vertex, color) = (vertexToVarMap Map.! vertex, color)
    let symEntryRegisterMap = Map.fromList $ map mapVarToColor $ Map.toList vertexColorMap
    return (symEntryRegisterMap, flowGraph)
    where
        -- | Gets the colors of the neighbours of a vertex in the current graph
        -- | Pre: we need to ensure that vertex v is already on the graph
        getNeighboursColors :: G.Vertex -> ColorState (Set.Set Color)
        getNeighboursColors vertex = do
            let isNeighbour (a, b) = vertex `elem` [a, b]
            let extractNeightbours (a, b) = if vertex == a then b else a
            neighbours <- Set.fromList . map extractNeightbours . filter isNeighbour . G.edges <$> getGraph
            colorMap <- getColoredVerticesMap
            return $ Set.map (colorMap Map.!) neighbours

        -- | Add a vertex to the current graph, attaching edges between vertices on it that
        -- | existed on the original interferenceGraph
        addVertex :: G.Vertex -> ColorState ()
        addVertex vertex = do
            currentVerticesSet <- Set.fromList . Map.keys <$> getColoredVerticesMap
            let edgeFilter (a, b) = (vertex == a && b `Set.member` currentVerticesSet) || (vertex == b && a `Set.member` currentVerticesSet)
            -- | We only care about edges that start/end on `vertex` and whose other vertex is already colored
            edgesToAddToCurrent <- Set.fromList . filter edgeFilter . G.edges . snd <$> getInterferenceGraph
            currentGraph <- getGraph
            let currentEdgesAsSet = Set.fromList $ G.edges currentGraph
            putGraph $ G.buildG (graphBounds currentGraph) $ Set.toList $ currentEdgesAsSet `Set.union` edgesToAddToCurrent

        -- | Pick a color from the available list that isn't already on the set supplied
        pickAColor :: Set.Set Color -> Color
        pickAColor = head . Set.toList . (availableColors `Set.difference`)

        -- | Assigns a color to a vertex, taking into account all of its current neighbours
        assignColor :: G.Vertex -> ColorState ()
        assignColor vertex = do
            addVertex vertex
            neighboursColors <- getNeighboursColors vertex
            let color = pickAColor neighboursColors
            colorVertex vertex color

-- | If graph coloration fails i.e. there are spilled vertices, we need to add spill code in order to minimize
-- | live ranges and make the interference graph less constrained
spill :: ColorState ()
spill = do
    (numberedBlocks, flowGraph) <- getProgramFlowGraph
    (vertexToVarMap, _) <- getInterferenceGraph
    spilledVertices <- Set.map (vertexToVarMap Map.!) <$> getSpilledVertices
    let newNumberedBlocks = map (spillBlock spilledVertices) numberedBlocks
    putProgramFlowGraph (newNumberedBlocks, flowGraph)
    where
        spillBlock :: Set.Set TACSymEntry -> NumberedBlock -> NumberedBlock
        spillBlock spilledVertices (blockId, tacs) = (blockId, concatMap (addSpill spilledVertices) tacs)

        addSpill :: Set.Set TACSymEntry -> TAC -> [TAC]
        -- | We don't have to spill a load or store
        addSpill _ tac@(ThreeAddressCode op _ _ _)
            | op `elem` [Load, Store] = [tac]
        addSpill spilledVertices tac =
            let isSpillVertex = filter (`Set.member` spilledVertices)
                makeLoad var = ThreeAddressCode Load (Just (Id var)) Nothing Nothing
                makeStore var = ThreeAddressCode Store (Just (Id var)) Nothing Nothing
                loadTacs = map makeLoad $ isSpillVertex $ Set.toList $ use1 tac
                storeTacs = map makeStore $ isSpillVertex $ Set.toList $ def1 tac
            in
                loadTacs ++ [tac] ++ storeTacs


-- | Actually runs chaitain/briggs optimistic coloring algorithm to produce new code with
-- | mappings between variables and their assigned registers
run :: FlowGraph -> Dictionary -> IO (RegisterAssignment, FlowGraph)
run flowGraph dict = State.evalStateT go initialState
    where
        initialState :: ColorationState
        initialState =
            let initialFlowGraph = initialStep flowGraph dict
            in ColorationState
                { csGraph = G.buildG (0, 1) [] -- dummy value
                , csDeletedVertices = Set.empty -- dummy value
                , csVertexStack = [] -- dummy value
                , csProgramFlowGraph = initialFlowGraph
                , csInterferenceGraph = (Map.empty, G.buildG (0, 1) []) -- dummy value
                , csLivenessInformation = [] -- dummy value
                , csProgramVariables = let (numberedBlocks, _) = initialFlowGraph in getProgramVariables numberedBlocks
                , csSpillsCostMap = Map.empty -- dummy value
                , csSpilledVertices = Set.empty -- dummy value
                , csColoredVertices = Map.empty -- dummy value
                }

        go :: ColorState (RegisterAssignment, FlowGraph)
        go = do
            build
            -- TODO: implement `coalesce`, we can live without it by the moment
            costs
            simplify
            spilledVertices <- getSpilledVertices

            -- We successfully found an assignment of registers that didn't caused spills
            if Set.null spilledVertices then
                select
            else spill >> go


printAllState :: ColorState ()
printAllState = do
    c <- State.get
    State.liftIO $ do
        putStrLn "Printing current graph"
        print $ csGraph c
        putStrLn "\nPrinting deleted vertices"
        putStrLn $ Set.showTree $ csDeletedVertices c
        putStrLn "\nPrint vertex stack"
        print $ csVertexStack c
        putStrLn "\nPrint spilled vertices"
        putStrLn $ Set.showTree $ csSpilledVertices c
        putStrLn "\nColored vertices"
        putStrLn $ showTree $ csColoredVertices c
        putStrLn $ "\nProgram variables " ++ show (length $ csProgramVariables c)
        putStrLn $ Set.showTree $ csProgramVariables c
        putStrLn "\nInterference graph"
        print $ let (_, graph) = csInterferenceGraph c in graph
        -- putStrLn "\nLiveness information"
        -- print $ csLivenessInformation c
        putStrLn "\nSpill cost map"
        putStrLn $ showTree $ csSpillsCostMap c
