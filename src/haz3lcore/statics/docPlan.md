### Current Plan to Implement Documentation Standard within the Hazel Codebase

- Use inline comments for complex logic
- Complete documnenting syn and ana for future contributors
- Clean up old code from previous iterations
- Create **.rei** files for interface
- Potential external documentation strategies when module system is introduced
- Create **.md** files for each directory that tracks documentation informations and maintains a to-do-list
- Things to think about
    - Format for inline comments

- TODO
    - Add sources to examples of well-documented code
    - Plan for summer code review
    - Motivation (why programmers must document their code)
        - [EECS 481](https://web.eecs.umich.edu/~weimerw/481/lectures/se-07-codereview.pdf)
        - [Wiki Code Review](https://en.wikipedia.org/wiki/Code_review)
        - [Microsoft](https://web.eecs.umich.edu/~weimerw/481/readings/codereview.pdf)


### Examples with Comments (To-Do)
``` ocaml
    let empty = fun lst ->
        match lst with 
        | [] -> true
        | _ -> false
```
