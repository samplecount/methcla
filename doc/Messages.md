# Messages

* **Node**
  * **free** :: NodeId -> IO () -- delete a Node
  * **set** :: NodeId -> [(Either Int String), Double)] -> IO ()  
     -- set control values
  * **map** :: NodeId -> [(Either Int String), (Either ControlBus AudioBus)] -> IO ()  
    -- map controls to buses
* **Group**
  * **new** :: NodeId -> AddAction -> NodeId -> IO ()  
    -- create new group
* **Synth**
  * **new** :: Uri -> NodeId -> AddAction -> NodeId -> [(Either Int String), Double] -> IO ()

Examples:

    [
        a msg:Set ;
        msg:subject [ node:id 12345 ] ;
        msg:body [
            nodeUri:symbol 3.14 ;
        ] ;
    ]
