# Creating a new exercise
1. Make a copy of src/haz3lweb/exercises/BlankTemplate.ml, filling in the arguments for your exercise. 
Make sure the module_name argument matches your module name. Use the .ml extension (for technical reasons).

2. Add your exercise to the exercise list in ExerciseSettings_base.re.

3. Compile and load Hazel, select your exercise, make sure Instructor Mode is on.

4. When satisfied, you can click the Export Exercise Module button in the top bar, which will generate
   a replacement for your .ml file that you can drop in (blowing away the code in the blank template is ok).

5. You will then have to create a prompt file, <module_name>_prompt.re. See examples for what goes there.

Once you're done, you can compile and run again and your exercise should be in the state you exported.
You can keep doing this cycle to tweak the exercise until it is ready.

# Changing things

Currently, if you need to change things like the hints, number of required tests, etc., you have to go 
find the corresponding record field in the giant .ml file that is generated. However, it would be nice 
to have a set of Exercise combinators that allow you to transform the spec to initialize it again and 
then go through the export cycle. If that sounds useful, please build it!

# Building

`make` and `make release` create instructor mode versions of Hazel.

`make student` and `make student-release` create student mode versions, which obviously don't let you enter student mode. 

Notably, student and instructor mode have a different serialization format, so it probably won't work to go 
between the two without clearing your local storage (in browser dev tools).

(This is also a QoL TODO that would be nice to resolve at some point.)

# Generating Grade Reports (for Gradescope, etc.)

1. Open the exercise in instructor mode and export a grading version (button in top bar) which generates an OCaml file.

2. Move the file to `src/haz3lschool/specs`.

3. Update the `src/haz3lschool/Specs.re` module with `<module_name>.exercise`.

4. Run `dune exec ./src/haz3lschool/gradescope.exe <path_to_student_json>` under project root to print the grade report.

To change the output format, adjust `Main.gen_grading_report` function in `Gradescope.re` .

