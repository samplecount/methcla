# Architecture

## Audio routing

```mermaid
graph TB
    HW_IN["Hardware Inputs"]
    HW_OUT["Hardware Outputs"]

    subgraph Engine
        ExtIn["External AudioBuses"]
        subgraph Root["Root Group"]
            subgraph Grp["Group"]
                S1["Synth"]
                S2["Synth"]
            end
        end
        Int["Internal AudioBuses"]
        ExtOut["External AudioBuses"]
    end

    HW_IN --> ExtIn
    ExtIn -->|read| S1
    ExtIn -->|read| S2
    S1 -->|write| Int
    Int -->|read| S2
    S1 -->|write| ExtOut
    S2 -->|write| ExtOut
    ExtOut --> HW_OUT
```

- **External AudioBuses** map to hardware I/O channels (input or output).
- **Internal AudioBuses** route audio between Synths within the Engine.
- The **Root Group** is the top of the node tree. Groups nest arbitrarily; Synths are leaves.
- Each Synth maps its audio inputs and outputs to buses via `/synth/map/input` and `/synth/map/output` (see [`osc-api.md`](osc-api.md)).
```
