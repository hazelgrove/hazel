## OS-Agnostic

- **Home** : Move(Extreme(Left(ByToken)))
- **End** : Move(Extreme(Right(ByToken)))
- **Escape** : Unselect
- **ArrowLeft** : Move(Local(Left(ByChar)))
- **ArrowRight** : Move(Local(Right(ByChar)))
- **ArrowUp** : Move(Local(Up))
- **ArrowDown** : Move(Local(Down))
- **Tab** : Put_down
- **Backspace** : Destruct(Left)
- **Delete** : Destruct(Right)
- **F2** : PrintZipperToConsole
- **F4** : Save
- **F6** : Load
- **F8** : LoadDefault
- **Shift Shift** : RotateBackpack

## OS-Agnostic (Meta is Ctrl on Windows, Cmd on Mac)

- **Meta + z** : Undo
- **Meta + Shift + Z** : Redo
- **Meta + x** : Pick_up
- **Meta + v** : Put_down
- **Meta + a** : SelectAll
- **Meta + num** : SwitchEditor(num)
- **Shift + ArrowLeft** : Select(Local(Left(ByToken)))
- **Shift + ArrowRight** : Select(Local(Right(ByToken)))
- **Shift + ArrowUp** : Select(Local(Up))
- **Shift + ArrowDown** : Select(Local(Down))
- **Alt** : ShowBackpackTargets
- **Alt + ArrowLeft if TargetsVisible** : MoveToBackpackTarget(Left(ByToken))
- **Alt + ArrowRight if TargetsVisible** : MoveToBackpackTarget(Right(ByToken))
- **Alt + ArrowUp** : MoveToBackpackTarget(Up)
- **Alt + ArrowDown** : MoveToBackpackTarget(Down)

## Mac-Specific

- **Cmd + ArrowLeft** : Move(Extreme(Left(ByToken)))
- **Cmd + ArrowRight** : Move(Extreme(Right(ByToken)))
- **Cmd + ArrowUp** : Move(Extreme(Up))
- **Cmd + ArrowDown** : Move(Extreme(Down))
- **Cmd + Shift + ArrowLeft** : Select(Extreme(Left(ByToken)))
- **Cmd + Shift + ArrowRight** : Select(Extreme(Right(ByToken)))
- **Cmd + Shift + ArrowUp** : Select(Extreme(Up))
- **Cmd + Shift + ArrowDown** : Select(Extreme(Down))
- **Alt + ArrowLeft** : Move(Local(Left(ByToken)))
- **Alt + ArrowRight** : Move(Local(Right(ByToken)))
- **Ctrl + a** : Move(Extreme(Left(ByToken)))
- **Ctrl + e** : Move(Extreme(Right(ByToken)))

##  PC-Specific

- **Ctrl + ArrowLeft** : Move(Local(Left(ByToken)))
- **Ctrl + ArrowRight** : Move(Local(Right(ByToken)))
- **Ctrl + Home** : Move(Extreme(Up))
- **Ctrl + End** : Move(Extreme(Down))
- **Ctrl + Shift + Home** : Select(Extreme(Up))
- **Ctrl + Shift + End** : Select(Extreme(Down))
- **Ctrl + Shift + ArrowLeft** : Select(Local(Left(ByToken)))
- **Ctrl + Shift + ArrowRight** : Select(Local(Right(ByToken)))
- **Ctrl + Shift + ArrowUp** : Select(Local(Up))
- **Ctrl + Shift + ArrowDown** : Select(Local(Down))
