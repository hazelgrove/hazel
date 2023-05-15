### Current Plan to Implement Documentation Standard within the Hazel Codebase

- Use inline comments for complex logic
- Code dependency tree for new members to navigate the codebase
- [Setup when2meet for weekly code review and cleanup meetings](https://neurocy.notion.site/Summer-2023-426d6d9051e24196bf7169a1e919fa53#:~:text=Team%20Devops%20and%20Cleanup)
- Complete documnenting syn and ana for future contributors
- Clean up old code from previous iterations
- Create  `.rei` files for interface
- Potential external documentation strategies when module system is introduced
- Create `.md` files for each directory that tracks documentation informations and maintains a to-do-list
- Things to think about
    - Format for inline comments
    - Character Limit
    - Lint Checking for ReasonML Code

- TODO
    - Add sources to examples of well-documented code
    - Plan for summer code review
    - Motivation (why programmers must document their code)
        - [EECS 481](https://web.eecs.umich.edu/~weimerw/481/lectures/se-07-codereview.pdf)
        - [Wiki Code Review](https://en.wikipedia.org/wiki/Code_review)
        - [Microsoft](https://web.eecs.umich.edu/~weimerw/481/readings/codereview.pdf)
    - **docs** and **cleanup** tags on GitHub Issues.
    - Map images from UI Elements to Code Modules
    - Code Style Guide
    - High Level overview of the codebase - `md`
    - Mapping UI to Code Modules


### Examples with Comments (To-Do)
``` ocaml
    let empty = fun lst ->
        match lst with 
        | [] -> true
        | _ -> false
```

```reason
    let (a,b) = (3,5) in a + b
```
