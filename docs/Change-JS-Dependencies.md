# Adding JavaScript Dependencies

Follow these steps to add and manage JavaScript dependencies in the project:

## 1. Add Dependencies to `package.json`
- Open the `package.json` file in the project root.
- Add the required dependencies under the `"dependencies"` section.  
    Example:
    ```json
    "dependencies": {
        "example-library": "^1.0.0"
    }
    ```

## 2. Update the Lock File
- Run the following command to install the new dependencies and update the `package-lock.json` file:
    ```bash
    make deps
    ```
    **Note:** This command runs `npm install` under the hood. It installs the dependencies listed in the `package.json` file and updates the `package-lock.json` file to reflect the exact versions of the installed packages. This ensures consistent dependency resolution across environments.

## 3. Reference Dependencies in JavaScript Files
- Make sure the desired dependency is referenced by `src/haz3lweb/www/prebundle.js`
    Example:
    ```javascript
    import exampleLibrary from 'example-library';

    exampleLibrary.doSomething();
    ```
- If you add a new toplevel js file (outside of prebundle.js) you need to ensure it is bundled and included
    - Ensure the `dune` build step is configured to use `esbuild` for bundling the JavaScript files.
        - This is the current step in `src/haz3lweb/dune`
            ```dune
            (rule
                (targets bundled.js)
                (action
                (run
                %{project_root}/node_modules/esbuild/bin/esbuild
                prebundle.js
                --bundle
                --outfile=bundled.js)))
            ```
        - We need the bundled js for every loaded js file we depend on
            - This means that dependencies should be added to already bundled files or a new bundle needs to be added. 
    - The dependencies will be bundled automatically during the build process.