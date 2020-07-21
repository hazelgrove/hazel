
source = []
for line in open('repl_source.dat', 'r'):
    line = line.strip().split('=')
    if line == ['#Processors']:
        source.append(None)
    elif len(line) == 2:
        source.append([token.strip() for token in line])

try:
    marker = source.index(None)
except:
    print("Marker not in file")
    raise
parsers, procesors = source[:marker], source[marker+1:]

out = []
for name, command in parsers:
    command = [token.strip() for token in command.split(';')]
    out.append('''    | "{}" =>
        let (v, inp) = {};
        (inp, [{},...stack])'''.format(name, *command))
for name, command in procesors:
    command = [token.strip() for token in command.split(';')]
    arguments, command = command[:-1], command[-1]
    out.append('''    | "{}" =>'''.format(name))
    for arg in arguments[::-1]:
        out.append('''        switch(stack) {{
        | [{},...stack] =>'''.format(arg))
    out.append('''            (inp, [{}, ...stack])'''.format(command))
    for arg in arguments:
        out.append('''        | [] => failwith("Empty stack")
        | _ => failwith("Type error")
        }''')

prefix = '''open Types;
open Parser;
open Printer;

let split(str) = {// :(
    let rec r(inp, out, live) = {
        switch(inp) {
        | [] => ([], [live, ...out], [])
        | [' ', ...inp] =>  r(inp, [live, ...out], [])
        | [c, ...inp] =>  r(inp, out, [c, ...live])
        }
    };
    let (_,out,_) = r(List.rev(explode(str)), [], [])
    List.map(implode, out)
}
let history = ref([]);
let process(inp:list(char), stack:list(debug_construct), command:string):(list(char), list(debug_construct)) = {
    history := [[(command, implode(inp), stack), ...List.hd(history^)], ...List.tl(history^)];
    switch(command) {'''
suffix = '''    | _ => failwith("Unknown command: \\""++command++"\\"")
    }
}
let main (inp:string, commands:string) = {
    //The module or file Str can't be found.
    //Reason can't keep up with OCaml. :(
    //let commands = Str.split(Str.regexp(" +"));
    let commands = split(commands);

    let inp = explode(inp);
    history := [[],...history^]
    let (inp, stack) = List.fold_left(
        ((inp, stack), command) => process(inp, stack, command),
        (inp, []),
        commands);
    history := [[("<print_all>", implode(inp), stack), ...List.hd(history^)], ...List.tl(history^)];
    List.iter(
        (construct) => Js.log(string_of_debug_construct(construct)),
        stack
    )
    let inp = implode(inp);
    if (inp != "") {
        Js.log("Warning, leftover input: \\""++inp++"\\"")
    };
}
'''

out = [prefix]+out+[suffix]
out = '\n'.join(out)
open('Repl.re', 'w').write(out)
