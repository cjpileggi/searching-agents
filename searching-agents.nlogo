;;-------------------------------------------------------------------------------- 
;;
;; Searching Agents
;; searching-agents.nlogo
;; 
;; A program which simulates a robot moving around on a grid world. Each patch is 
;; a square in the grid, and may contain an obstacle. The robot is trying to find
;; its way to a goal patch. Includes the breadth-first search, greedy search, and 
;; the A* search 
;;


;;--------------------------------------------------------------------------------
;;
;; We have one breed and a few global variables

breed [robots robot]
globals [g-xcor g-ycor r-xcor r-ycor r-plan done nodes-on-agenda nodes-visited]

;; Values given to each patch
;; One value is used to give each patch its own heuristic value, and another is
;; used to identify the patch's path cost
patches-own[h-val path-cost]


;;--------------------------------------------------------------------------------
;;
;; Code to set up the environment

;; setup
;;
;; To setup we get rid of any existing robots, color the background, pick obstacle
;; locations and a goal, and then put the robot down in a random position

to setup
  clear-all
  ask turtles [die]
  reset-ticks
  paint-patches
  generate-obstacles
  set-goal
  position-robot
  set r-plan []
  set done false
  clear-plot
  set nodes-on-agenda 0
  set nodes-visited 0
end

;; paint-patches

to paint-patches
  ask patches [set pcolor green]
end

;; generate-obstacles

to generate-obstacles
  repeat obstacles [ask one-of patches [set pcolor black]]
end

;; set-goal

to set-goal
  ask one-of patches [
    set pcolor red
    set g-xcor pxcor
    set g-ycor pycor
    ]
end

;; position-robot

to position-robot
  
  ;; setup the robot breed
  
  set-default-shape robots "robot"
  create-robots 1 [set color black]
  
  ;; place the robot
  
    ;; remember the initial location of the robot to allow us to reset
  
  set r-xcor free-x-cor
  set r-ycor free-y-cor

  ask robots [set xcor r-xcor]
  ask robots [set ycor r-ycor]
  ask robots [set heading 0]
    
  ;; make sure there isn't an obstacle at the robot's initial position
  
  ask patches [if ((pxcor = r-xcor) and (pycor = r-ycor)) [
    set pcolor green
  ]]
end

;; free-x-cor free-y-cor
;;
;; These are supposed to generate random locations that aren't obstacles. For
;; now they just report 0 or a random location (depending on the value of 
;; robot-position, and we adjust the obstacles to avoid them.

to-report free-x-cor
  if-else robot-position = "center"[
    report 0
  ]
  [
    report random-pxcor
  ]
end


to-report free-y-cor
  if-else robot-position = "center"[
    report 0
  ]
  [
    report random-pycor
  ]
end

;; reset
;;
;; Reset allows you to return the robot to its initial position to run more
;; than once in the same environment. Settings can be reapplied

to reset
  ask robots [
    set xcor r-xcor
    set ycor r-ycor
  ]
  reset-ticks
  set r-plan []
  set done false
  repaint-space
  clear-plot
  set nodes-on-agenda 0
  set nodes-visited 0
end

;; repaint-space
;;
;; In case we have been looking at the search, 

to repaint-space
ask patches [
  if (pcolor != black) and (pcolor != red)[ 
    set pcolor green
    
  ]  
]
end

;;--------------------------------------------------------------------------------
;;
;; 

;; go
;;
;; The top level loop. Make the robot take a step forward, tick, and stop if the 
;; robot is at the goal (which stops the clock).

to go
  run-robot
  tick
  ask robots [at-goal-test]
  if done[
    stop
  ]
end

;; run-robot
;;
;; The high level call to make the robots move --- as long as you aren't at the 
;; goal, make a move.

to run-robot 
  ask robots [
    if-else not at-goal [
      robot-move
    ]
    [
      set color green
    ]
  ]
end

;; robot-move
;;
;; The place where we plug in different ways to move the robot. The basic
;; mechanism is just to make a random move.

to robot-move
  if-else movement = "random" [
    robot-random-move  
  ]
  [
    robot-planned-move
  ]
end

;; robot-random-move
;;
;; Pick a direction, randomly and with equal probability

to robot-random-move
 let direction random 4
 ;;show direction
 if direction = 0 [
   move "north"
 ]
 if direction = 1 [
   move "south"
 ]
 if direction = 2 [
   move "east"
 ]
 if direction = 3 [
   move "west"
 ]
end

;; move
;;
;; You can move in a direction as long as there is no obstacle.

to move [direction]
  ask robots [
    if not obstacle direction patch-here[
      step direction
    ]
  ]
end

;; obstacle
;;
;; We spot an obstacle by identifying if there is a black patch in the 
;; requisite direction, or if we are against the edge of the world.

to-report obstacle[direction my-patch]

  let a-color green
  
  ;; First look at the color of the patch in the right direction, taking care of 
  ;; the edges of the field, and repeat for all four directions.
  
  if direction = "north" [
    if-else patch ([pxcor] of my-patch) (([pycor] of my-patch) + 1) = nobody [  
      set a-color black            
    ]
    [ 
      set a-color [pcolor] of patch ([pxcor] of my-patch) (([pycor] of my-patch) + 1)
    ]
  ]
  if direction = "south" [
     if-else patch ([pxcor] of my-patch) (([pycor] of my-patch) - 1) = nobody [  
      set a-color black
    ]
    [ 
      set a-color [pcolor] of patch ([pxcor] of my-patch) (([pycor] of my-patch) - 1)
    ]
  ]
  if direction = "east" [
     if-else patch (([pxcor] of my-patch) + 1) ([pycor] of my-patch) = nobody [  
      set a-color black
    ]
    [ 
      set a-color [pcolor] of patch (([pxcor] of my-patch) + 1) ([pycor] of my-patch)
    ]
  ]
  if direction = "west" [
    if-else patch (([pxcor] of my-patch) - 1) ([pycor] of my-patch) = nobody [  
      set a-color black
    ]
    [     
      set a-color [pcolor] of patch (([pxcor] of my-patch) - 1) ([pycor] of my-patch)
    ]
  ]
  
  ;; Then report based on the color
  
  if-else a-color = black [
    report true
  ]
  [
    report false
  ]
end

;; step
;;
;; Move in the requisite direction, making each step exactly one patch.

to step [direction]
  if direction = "north" [
    set ycor [pycor] of patch-at 0 1
  ]
  if direction = "south" [
    set ycor [pycor] of patch-at 0 -1
  ]
  if direction = "east" [
    set xcor [pxcor] of patch-at 1 0
  ]
  if direction = "west" [
    set xcor [pxcor] of patch-at -1 0
  ]
end

;; at-goal-test
;;
;; robot signals it has got to the goal so that we can stop the simulation

to at-goal-test
  if is-goal patch-here [
    set done true
  ]
end

;; at-goal
;;
;; robot knows it has reached the goal

to-report at-goal
  if-else is-goal patch-here [
    report true
  ]
  [
    report false
  ]
end

;; is-goal
;;

to-report is-goal [my-patch]
  let x-cor [pxcor] of my-patch
  let y-cor [pycor] of my-patch 
  if-else ((x-cor = g-xcor) and  (y-cor = g-ycor))[
    report true
  ]
  [
    report false
  ]
end

;;---------------------------------------------------------------------------
;;
;; Searching Functions

;; robot-planned-move
;;
;; if r-plan is set, then we have found a plan, so we follow it. Otherwise we
;; look for a plan.
;;
;; When we look, we see what type of search to use based on the value set for
;; "movement" 

to robot-planned-move
    if-else r-plan = []
  [
    if movement = "breadth-first-search" [find-plan]
    if movement = "greedy-search" [find-greedy-plan]
    if movement = "a*-search" [find-a*-plan]
  ]
  [
    follow-plan
  ]
end

;; follow-plan
;;
;; If we have a plan we follow it. A plan is a list of patches. To follow the plan
;; we take the first patch off the list and move to its x and y coordinates. Because
;; of the way the plan was assembled, this will always be a single step from where
;; we are.

to follow-plan
   if r-plan != [][
     let target first r-plan
     set r-plan but-first r-plan
     set xcor [pxcor] of target
     set ycor [pycor] of target
     if (show-path and not is-goal patch-here)[
       ask patch-here [set pcolor magenta]
     ]
   ]  
end

;; find-plan
;; 
;; Top-level function for the breadth-first search planning. Delegates all the real work to get-plan, but
;; checks if no plan is found (if the agenda held by get-plan ever becomes empty) and
;; controls plan execution by only setting r-plan if we have a non-null plan.
;;
;; Also prints the plan out.

to find-plan
  
  let plan (get-plan (list patch-here) [] (list (list patch-here)))
  
  if-else empty? plan [
    show "failed"
    set done true
  ]
  [
    show plan
    set r-plan plan
  ]
end

;; find-greedy-plan
;;
;; Top-level function for finding a plan using greedy search. It calls get-greedy-plan to search the world
;; for the goal and returns the visited nodes. If the goal is found, they are given to set-plan to give 
;; the robot its plan. Otherwise, it returns a failure.

to find-greedy-plan
  
  let visited-nodes (get-greedy-plan (list patch-here) (list patch-here) [])
  
  if-else empty? visited-nodes [
    show "failed"
    set done true
  ]
  [
    let plan (set-plan (list patch-here) [] visited-nodes (list(list patch-here)))
    show plan
    set r-plan plan
  ]
end

;; find-a*-plan
;;
;; Top-level function for finding a plan using the A* search. It sets the robot's initial patch a path cost of 0 and
;; calls get-a*-plan to search the world for the goal. If it is found, it gives the visited nodes to set-plan 
;; to set the robot's plan. Otherwise it returns a failure. 

to find-a*-plan
  
  ask patch-here[set path-cost 0]
  let visited-nodes (get-a*-plan (list patch-here) (list patch-here) [])
  
  if-else empty? visited-nodes [
    show "failed"
    set done true
  ]
  [
    let plan (set-plan (list patch-here) [] visited-nodes (list(list patch-here)))
    show plan
    set r-plan plan
  ]
end
;; get-plan
;;
;; Does a simple breadth-first search through the navigable squares until
;; it finds the goal, and either returns the empty list, indicating it can't find
;; the goal, or a list of nodes that trace a path from the start point to the goal.
;;
;; The function is recursive, and calls itself for each new step in the plan. 
;; It only causes a stack overflow when we get to 30x30 worlds and we 
;; are visiting 2500+ nodes.

to-report get-plan[agenda visited-nodes plans]
   ;; if the agenda is empty, we have failed to find the goal
   if-else empty? agenda[
     report []
   ]
   [
     ;; otherwise take the front node off the agenda and if it is the goal, we stop searching
     let front first agenda
       if-else is-goal front [
         report first plans
       ]
       [
         ;; otherwise, remember we visited that node and find the nodes we can get to from it.
         set visited-nodes (lput front visited-nodes)
         ;;show visited-nodes
         let new-nodes (find-surrounding-nodes front)
         ;; we scour these nodes to remove ones we have visited before, or ones that are already
         ;; on the agenda.
         foreach visited-nodes [
           set new-nodes remove ? new-nodes
         ]  
         foreach agenda [
           set new-nodes remove ? new-nodes
         ]  
         ;; then, since these are new nodes, we remember how to get to them, and create
         ;; a new agenda. We discard the current front of the agenda, and the plan stub that
         ;; got us to that point (since it is now contained in a longer plan.
         set plans (append (but-first plans) (add-nodes-to-plan (first plans) new-nodes))
         set agenda (append (but-first agenda) new-nodes)
  
         ;; plot the number of visited nodes, and color patches if we chose to do that and
         ;; set the values that get reported to the interface
         if show-agenda [
           foreach agenda [ask ? [if (not is-goal self)[set pcolor blue]]]
         ]
         if-else show-visited [
           foreach visited-nodes [ask ? [if (not is-goal self)[set pcolor yellow]]]
         ]
         [ ;; if we aren't showing the visited nodes, but we are showing the agenda, then we have 
           ;; to set the visited nodes green so that they don't stay blue once  they move off the
           ;; agenda
           if show-agenda [
           foreach visited-nodes [ask ? [if (not is-goal self)[set pcolor green]]]
           ]
         ]
         set-current-plot-pen "visited nodes" ;; which pen we want to use next
         plot length visited-nodes
         set nodes-on-agenda length agenda
         set nodes-visited length visited-nodes
         
         report (get-plan agenda visited-nodes plans)
       ]                 
   ]
end

;; get-greedy-plan
;;
;; Does a greedy search. It uses the same methods as the breadth-first search, but includes a function
;; that evaluates the heuristic values of the nodes on the agenda. It then puts the nodes
;; with the best values on the g-agenda. It colours the nodes according to the agenda nodes values. When it finishes,
;; it either returns an empty list if it fails to find the goal, or the list of visited nodes if successful. 

to-report get-greedy-plan[agenda g-agenda visited-nodes]
   ;; if the agenda is empty, we have failed to find the goal
   if-else empty? agenda[
     report []
   ]
   [
     ;; otherwise take the front node off the g-agenda and if it is the goal, we stop searching
     let front first g-agenda 
       if-else is-goal front [
         set visited-nodes (lput front visited-nodes)
         report visited-nodes
       ]
       [
         ;; otherwise, remember we visited that node and find the nodes we can get to from it.
         set visited-nodes (lput front visited-nodes)
         
         ;; search for new nodes
         let new-nodes (find-surrounding-nodes front)
         ;; we scour these nodes to remove ones we have visited before, or ones that are already
         ;; on the agenda. Double Check that all agenda and g-agenda nodes were never visited.
         foreach visited-nodes [
           set new-nodes remove ? new-nodes
           set agenda remove ? agenda
           set g-agenda remove ? g-agenda
         ]  
         foreach agenda [ 
           set new-nodes remove ? new-nodes
         ]  
          
         ;; then, since these are new nodes, we remember how to get to them, and create
         ;; a new agenda. We discard the current front of the agenda
            
         set agenda (append (agenda) new-nodes)
         
         ;; put the agenda nodes with the best heuristic value on g-agenda
         set g-agenda ((best-g-nodes agenda g-agenda))

         ;; plot the number of visited nodes, and colour nodes if we chose to do that and
         ;; set the values that get reported to the interface
         
         ;; set the colours of the agenda nodes according to their heuristic value. The greater the value,
         ;; the darker blue the node is.
         if show-agenda [
           let best 999
           foreach agenda[ ask ?[ if(best >= h-val)[ set best h-val]]]
           
           foreach agenda [
             ask ? [
              if (h-val = best)[ 
               if (not is-goal self)[set pcolor 101]]
              
              if ((h-val > best) and (h-val < best + 1.5))[
                if (not is-goal self)[set pcolor 102]]
              
              if ((h-val >= best + 1.5) and (h-val < best + 2))[
                if (not is-goal self)[set pcolor 103]]
              
              if ((h-val >= best + 2) and (h-val < best + 5))[
                if (not is-goal self)[set pcolor 104]]
              
              if ((h-val >= best + 5) and (h-val < best + 10))[
                if (not is-goal self)[set pcolor 105]]
 
              if ((h-val >= best + 10) and (h-val < best + 15))[
                if (not is-goal self)[set pcolor 106]] 
              
              if ((h-val >= best + 20) and (h-val < best + 25))[
                if (not is-goal self)[set pcolor 107]] 
              
              if (h-val >= best + 25)[
                if (not is-goal self)[set pcolor 108]] 
              ]
             ]
           ]
         ]
         
         if-else show-visited [
           foreach visited-nodes [
             ask ? [if (not is-goal self)[set pcolor yellow]  
               ]
             ]
    
         ]
         [ ;; if we aren't showing the visited nodes, but we are showing the agenda, then we have 
           ;; to set the visited nodes green so that they don't stay blue once  they move off the
           ;; agenda
           if show-agenda [
           foreach visited-nodes [ask ? [if (not is-goal self)[set pcolor green]]]
           ]
         ]

         set-current-plot-pen "visited nodes" ;; which pen we want to use next
         plot length visited-nodes
         set nodes-on-agenda length agenda
         set nodes-visited length visited-nodes
         
         report (get-greedy-plan agenda g-agenda visited-nodes)
       ]                 
   
end

;; best-g-nodes
;;
;; Gives the agenda nodes their heuristic values for the greedy search. Finds the distance between 
;; each agenda node and the goal and gives those values to each node's h-val values. 
;; Adds the nodes with the best values onto the g-agenda
to-report best-g-nodes [agenda g-agenda]
let best 999

foreach agenda[
  let ag-xcord [pxcor] of ?
  let ag-ycord [pycor] of ?
  
  let dist line-dist g-xcor ag-xcord g-ycor ag-ycord
  
  ask ?[set h-val dist]
  
  if (best >= dist)[
   
   set best dist
  ]
]

foreach agenda[ 
 
 ask ?[
  
  if (h-val = best)[
    set g-agenda (lput ? g-agenda)
  ]
 ]
]

report g-agenda
  
end

;; get-a*-plan
;;
;; Does an A* search. It uses the same methods as the greedy search, but includes a function
;; that gives the nodes on the agenda their heuristic values and adds their path costs. 
;; It then puts the nodes with the best values on the a-agenda. It either 
;; returns an empty list if it failed to find the goal, or the visited nodes if successful. The node where 
;; the robot is located is given a path cost of 0 since it is the initial node. 
;; Each succeeding node has a path cost of 1

to-report get-a*-plan[agenda a-agenda visited-nodes]
   ;; if the agenda is empty, we have failed to find the goal 
   if-else empty? agenda[
     report []
   ]
   [
     ;; otherwise take the front node off the agenda and if it is the goal, we stop searching
     let front first a-agenda
       if-else is-goal front [
         
         set visited-nodes (lput front visited-nodes)
         report visited-nodes 
       ]
       [ 
         ;; otherwise, remember we visited that node and find the nodes we can get to from it.
         set visited-nodes (lput front visited-nodes)
         ;;show visited-nodes
         let new-nodes (find-surrounding-nodes front)
         
         ;; set the path cost of the new nodes. Since the path cost of each patch is 1, it adds
         ;; 1 to the value of the "front" node and gives that value to each new node
         let prev 0
         ask front[set prev path-cost]
         
         foreach new-nodes[
           ask ?[set path-cost (prev + 1)]
         ]
         ;; we scour these nodes to remove ones we have visited before, or ones that are already
         ;; on the agenda.
         foreach visited-nodes [
           set new-nodes remove ? new-nodes
           set agenda remove ? agenda
           set a-agenda remove ? a-agenda
         ]  
         foreach agenda [
           set new-nodes remove ? new-nodes
         ]  
         
         ;; then, since these are new nodes, we remember how to get to them, and create
         ;; a new agenda. We then search the agenda for the nodes with the best heuristic value
         ;; and put them on a-agenda
         set agenda (append (agenda) new-nodes)
         set a-agenda ((best-a-nodes agenda a-agenda))
        
         ;; plot the number of visited nodes, and color patches if we chose to do that and
         ;; set the values that get reported to the interface
         
         ;; use the same coloring method as get-greedy-plan 
         if show-agenda [
            let best 999
           foreach agenda[
             ask ?[ if(best >= h-val)[ set best h-val]]
           ]
           
           foreach agenda [
             ask ? [
              if (h-val = best)[ 
               if (not is-goal self)[set pcolor 101]]
              
              if ((h-val > best) and (h-val < best + .1))[
                if (not is-goal self)[set pcolor 102]]
              
              if ((h-val >= best + .1) and (h-val < best + .2))[
                if (not is-goal self)[set pcolor 103]]
              
              if ((h-val >= best + .3) and (h-val < best + .5))[
                if (not is-goal self)[set pcolor 104]]
              
              if ((h-val >= best + .5) and (h-val < best + .7))[
                if (not is-goal self)[set pcolor 105]]
              
              if ((h-val >= best + .7) and (h-val < best + .9))[
                if (not is-goal self)[set pcolor 106]]
 
              if ((h-val >= best + .9) and (h-val < best + 1.2))[
                if (not is-goal self)[set pcolor 107]] 
              
              if (h-val >= best + 1.2)[
                if (not is-goal self)[set pcolor 108]] 
             ]
            ]
         ]
         if-else show-visited [
           foreach visited-nodes [
             ask ? [if (not is-goal self)[set pcolor yellow]]
             ]
         ]
         [ ;; if we aren't showing the visited nodes, but we are showing the agenda, then we have 
           ;; to set the visited nodes green so that they don't stay blue once  they move off the
           ;; agenda
           if show-agenda [
           foreach visited-nodes [ask ? [if (not is-goal self)[set pcolor green]]]
           ]
         ]         

         set-current-plot-pen "visited nodes" ;; which pen we want to use next
         plot length visited-nodes
         set nodes-on-agenda length agenda
         set nodes-visited length visited-nodes
         
         report (get-a*-plan agenda a-agenda visited-nodes)
       ]                 
   ]
end

;; best-a-nodes
;;
;; Gives each node on the agenda its heuristic value for the A*. It finds the distance between
;; the goal node and the agenda nodes, and then adds their values to their individual path costs.
;; The best is put on the a-agenda 
to-report best-a-nodes [agenda a-agenda]
let best 999

foreach agenda[
  let ag-xcord [pxcor] of ?
  let ag-ycord [pycor] of ?
  
  let dist line-dist g-xcor ag-xcord g-ycor ag-ycord
  
  let total-cost 0
  
  ask ?[set h-val (dist + path-cost)]
  ask ?[set total-cost h-val]
  
  if (best >= total-cost)[
   
   set best total-cost
  ]
]

foreach agenda[ 
  
  ask ?[
  
  if (h-val = best)[
    set a-agenda (lput ? a-agenda)
   ]
  ]
]

report a-agenda
  
end

;; set-plan
;;
;; Used to find the plans for the greedy search and A* search. It uses the same methods for plan making for the
;; breadth-first search. Since nodes are always removed from the greedy and A* agendas, and are not in a consistent
;; order, the plan search cannot be used within the search function. Instead, this function takes the visited nodes
;; returned from the searches, and uses a breadth-first search within those nodes to find a plan. If the goal was 
;; found during the searches, it will return the plans to get to it.
 
to-report set-plan[agenda acknowledged-nodes visited-nodes plans]
   ;; if the agenda is empty, we have failed to find the goal
   if-else empty? agenda[
     report [] 
   ]
   [
     ;; otherwise take the front node off the agenda and if it is the goal, we stop searching
     let front first agenda
       if-else is-goal front [
         report first plans 
       ] 
       [
         ;; otherwise, remember we already acknowledged that node and find the nodes we can get to from it.
         set acknowledged-nodes (lput front acknowledged-nodes)
         ;;show visited-nodes
         let new-nodes (find-surrounding-nodes front)
         
         ;; we scour these nodes to remove ones that were not visited visited before, or ones that are already
         ;; on the agenda, or the nodes that were already acknowledged.
         
         foreach new-nodes [
          if not (member? ? visited-nodes)[ set new-nodes remove ? new-nodes]
         ]
         
         foreach acknowledged-nodes[ set new-nodes remove ? new-nodes]
          
         foreach agenda[set new-nodes remove ? new-nodes]
         
           
         ;; then, since these are new nodes, we remember how to get to them, and create
         ;; a new agenda. We discard the current front of the agenda, and the plan stub that
         ;; got us to that point (since it is now contained in a longer plan)
         set plans (append (but-first plans) (add-nodes-to-plan (first plans) new-nodes))
         set agenda (append (but-first agenda) new-nodes)
         
         ;; call again
         report (set-plan agenda acknowledged-nodes visited-nodes plans)
       ]                 
   ]
end

;; find-surrounding-nodes
;;
;; given a patch and a list of patches, finds the patches north, south, east
;; and west of the given patch that are in the field and not obstacles, and
;; returns them

to-report find-surrounding-nodes [my-patch]
  let node-list []
  let new-patch nobody
  if not obstacle "north" my-patch [
    ask my-patch [set new-patch patch-at 0 1]
    if new-patch != nobody [
      set node-list lput new-patch node-list
    ]
  ]
  set new-patch 0
  if not obstacle "south" my-patch [
    ask my-patch [set new-patch patch-at 0 -1]
    if new-patch != nobody [
      set node-list lput new-patch node-list
    ]
  ]
  set new-patch 0
  if not obstacle "east" my-patch [
    ask my-patch [set new-patch patch-at 1 0]
    if new-patch != nobody [
      set node-list lput new-patch node-list
    ]
  ]
  set new-patch 0
  if not obstacle "west" my-patch [
    ask my-patch [set new-patch patch-at -1 0]
    if new-patch != nobody [
      set node-list lput new-patch node-list
    ]
  ]
  report node-list
end

;; add-nodes-to-plan
;;
;; Take each node in nodes in turn and add it to the end of the current plan

to-report add-nodes-to-plan [plan nodes]
  let new-plans []
  foreach nodes [
    let new-plan lput ? plan
    set new-plans lput new-plan new-plans
  ]
  report new-plans
end

;; remove-redundant-plans
;;
;; Given a node that we have already visited, and a set of plans, remove the plan to
;; take us to the node --- the node is redundant, and we don't want to keep plans that
;; won't be helpful.

to-report remove-redundant-plans [node plans]
  let revised-plans []
  foreach plans [
    if (last ?) != node [  
      set revised-plans lput ? revised-plans
    ]
  ]
  report revised-plans
end

;; append
;; 
;; Join two lists together, tacking its second argument onto the end of the first 
;; argument.
;;
;; I have it on good authority that this basic list handling functionality is in Netlogo
;; under another name, but I can't find it.

to-report append [list1 list2]
  foreach list2 [
    set list1 lput ? list1
  ]
  report list1
end

;; line-dist
;;
;; calculates the distance between 2 nodes
to-report line-dist [x1 x2 y1 y2]
  let dist sqrt (((x1 - x2)*(x1 - x2)) + ((y1 - y2)*(y1 - y2)))
  report dist
end
  
  
@#$#@#$#@
GRAPHICS-WINDOW
261
10
832
602
25
25
11.0
1
10
1
1
1
0
0
0
1
-25
25
-25
25
0
0
1
ticks
30.0

BUTTON
26
10
92
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
25
48
220
81
obstacles
obstacles
0
1000
750
1
1
NIL
HORIZONTAL

BUTTON
26
190
89
223
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
32
231
89
276
NIL
ticks\n
0
1
11

BUTTON
104
188
167
221
NIL
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
25
86
219
131
movement
movement
"random" "breadth-first-search" "greedy-search" "a*-search"
3

SWITCH
95
279
226
312
show-agenda
show-agenda
0
1
-1000

SWITCH
95
322
226
355
show-visited
show-visited
0
1
-1000

CHOOSER
25
137
219
182
robot-position
robot-position
"center" "random"
1

PLOT
30
360
190
480
visited nodes
time
nodes
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"visited nodes" 1.0 0 -16777216 true "" ""

MONITOR
30
487
108
532
agenda
nodes-on-agenda
0
1
11

MONITOR
112
487
190
532
visited
nodes-visited
17
1
11

SWITCH
96
237
226
270
show-path
show-path
0
1
-1000

@#$#@#$#@
# Christopher Pileggi-Project 1

### Description

The heuristic that I used for the Greedy and A* searches: 
h(n) = the shorter the distance between the goal node and a specific node, the greater the value that specific node has. 

For each node on the agenda, I used the distance formula to find the distances between them and the goal. Those distances are their individual heuristic values. The node with the shortest distance to the goal has the best heuristic value.

The greedy search strictly uses this heuristic in its search. It gives the agenda nodes their heuristic values and those with the best are searched and expanded.

The A* search uses the heuristic to give a node its value, and also takes the path cost into consideration. In this case, the path cost is the number of nodes the robot must visit to get to a specific node. Each node has a path cost of 1. The search uses the heuristic to give a node its heuristic value, and then adds the node's path cost to that value. The node with the the lowest overall value on the agenda is the best and is the next to be searched and expanded.  

Additional feature: 
The color of the agenda nodes is determined by their values. The better the value of a node is compared to the other nodes on the agenda, the darker blue it is.

## Experiment
### Breadth-first Search

Trial 1 : 1892
Trial 2 : 1647
Trial 3 : 261
Trial 4 : 1086
Trial 5 : 1718
Trial 6 : 853
Trial 7 : 167
Trial 8 : 1656
Trial 9 : 742
Trial 10 : 1696
Trial 11 : 1566
Trial 12 : 855
Trial 13 : 1855
Trial 14 : 1506
Trial 15 : 442
Trial 16 : 1590
Trial 17 : 797
Trial 18 : 1041
Trial 19 : 1620
Trial 20 : 1064

Average = 1202.7 (about 1203 nodes)

Standard Deviation = +/- 541.81 nodes


### Greedy Search

Trial 1 : 34
Trial 2 : 48
Trial 3 : 91
Trial 4 : 99
Trial 5 : 76
Trial 6 : 23
Trial 7 : 69
Trial 8 : 28
Trial 9 : 49
Trial 10 : 61
Trial 11 : 44
Trial 12 : 53
Trial 13 : 36
Trial 14 : 39
Trial 15 : 31
Trial 16 : 70
Trial 17 : 45
Trial 18 : 48
Trial 19 : 52
Trial 20 : 50

Average = 52.3 (about 52 nodes)

Standard Deviation = +/- 20.22 nodes

### A* Search

Trial 1 : 434
Trial 2 : 334
Trial 3 : 406
Trial 4 : 219
Trial 5 : 341
Trial 6 : 139
Trial 7 : 642
Trial 8 : 217
Trial 9 : 1002
Trial 10 : 906
Trial 11 : 337
Trial 12 : 217
Trial 13 : 504
Trial 14 : 688
Trial 15 : 67
Trial 16 : 251
Trial 17 : 170
Trial 18 : 263
Trial 19 : 398
Trial 20 : 111

Average = 382.3 (about 382 nodes)

Standard Deviation = +/- 254.81 nodes

## Evaluation

Of the three searches, the breadth-first search visits the greatest number of nodes on average. The location of the goal node is not taken into consideration and the search visits and expands every node on the agenda, even if they are irrelevant to the search. As a result, the number of nodes visited increases exponenially, which is why the average is so big.

Of the three searches, the greedy search visits the least number of nodes on average. Because of the heuristic, the search tends to visit the nodes that are in the direction of the goal. As a result, it usually creates a path that leads directly to the goal and therefore, a small number of nodes are visited.

On average, the A* search will visit a greater number of nodes than the greedy search, but far less than that of the breadth-first search. It also has a relatively large standard deviation compared to its average. The search tends to visit nodes in the direction of the goal, but also visits nodes that may not be directly in the goals path. In fact, it appears that at certain points during the search, some nodes that are closer to the robot have a better value than nodes that are closer to the goal. However, when the search finds the goal, it usually provides a shorter path for the robot than the greedy search does.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

robot
true
0
Circle -7500403 false true 2 2 297
Line -7500403 true 150 0 150 105

robot2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Rectangle -7500403 true true 150 30 150 135
Rectangle -7500403 true true 135 30 160 138

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
