# Copilot Instructions for Playground Monorepo

## Workspace Overview

This is a **multi-language learning monorepo** containing independent experimental projects. Each directory is a standalone project with its own tooling, dependencies, and development workflow. Projects are not interconnected—changes in one do not affect others.

### Project Structure

```
playground/
├── ada/           # Ada programming experiments (medium/hard difficulty)
├── hello_pi/      # Flutter/Dart mobile app (default template)
├── Hemp-app/      # React + TypeScript + Vite (canvas-based dog tribute)
├── hemp-django/   # Django/Python (minimal pygame project)
├── jesus-loves-all/ # React + TypeScript + Vite (children illustration)
└── typescript/    # TypeScript learning experiments
```

## Development Workflows

### React/TypeScript Projects (Hemp-app, jesus-loves-all)
- **Stack**: React 19.x + TypeScript 5.9.x + Vite 7.x + ESLint
- **Dev server**: `npm run dev` (starts Vite dev server with HMR)
- **Build**: `npm run build` (runs `tsc -b && vite build`)
- **Lint**: `npm run lint` (ESLint with react-hooks and react-refresh plugins)
- **Preview**: `npm run preview` (preview production build)
- **Key dependencies**: babel-plugin-react-compiler for React optimizations
- **Note**: Always run `npm install` when switching between React projects

### Flutter Project (hello_pi)
- **Stack**: Flutter with Dart SDK ^3.10.0
- **Dev**: `flutter run` (hot reload with 'r', hot restart with 'R')
- **Build**: `flutter build <platform>` where platform = web, apk, appbundle, ios, macos, linux, windows
- **Test**: `flutter test`
- **Deps**: `flutter pub get` to install/update dependencies from [pubspec.yaml](hello_pi/pubspec.yaml)
- **Platforms**: Configured for all platforms (web, Android, iOS, macOS, Windows, Linux)
- **Main entry**: [lib/main.dart](hello_pi/lib/main.dart) - standard counter demo app

### Python Project (hemp-django)
- **Stack**: Python with pygame
- **Setup**: `pip install -r requirements.txt` (installs pygame only)
- **Run**: `python hemp-danjo.py`
- **Assets**: Stored in `assets/` directory

### TypeScript Experiments (typescript/)
- **Compile**: `tsc <filename>.ts` (outputs .js file)
- **Direct execution**: `ts-node <filename>.ts` (requires ts-node installed globally)
- **Config**: [tsconfig.json](typescript/tsconfig.json) uses strict mode, module=nodenext, target=esnext
- **Game demo**: Contains a subdirectory `game-demo/` with HTML/TypeScript game experiments

### Ada Projects (ada/)
- **Structure**: Organized by difficulty (`medium/`, `hard/`)
- **Example**: [hard/log_analyzer.adb](ada/hard/log_analyzer.adb) - complex log analysis with hashed maps and vectors
- **Compile**: `gnatmake <filename>.adb`
- **Run**: `./<filename>` after compilation

## Project-Specific Patterns

### React Projects Convention
- **Canvas usage**: Hemp-app uses HTML5 Canvas with refs ([src/App.tsx](Hemp-app/src/App.tsx) lines 1-30)
- **CSS custom properties**: jesus-loves-all uses CSS variables for dynamic styling ([src/App.tsx](jesus-loves-all/src/App.tsx) lines 11-17)
- **Component structure**: Functional components with TypeScript, using React 19 features
- **ESLint config**: Both projects use flat config format (eslint.config.js)

### Flutter Conventions
- **Material Design**: Uses Material 3 color schemes with ColorScheme.fromSeed
- **State management**: StatefulWidget pattern for interactive UI
- **Hot reload workflow**: Emphasize hot reload (⌘S/Ctrl+S) over hot restart for state preservation

### Ada Code Style
- **Naming**: Ada_Case for procedures/functions, UPPER_CASE for constants
- **Packages**: Heavy use of Ada.Containers (Hashed_Maps, Vectors) for data structures
- **Error handling**: Uses Ada.Command_Line for CLI argument processing

## Important Notes

1. **No shared dependencies**: Each project manages its own node_modules/, build outputs, and dependencies
2. **Educational purpose**: Code may not follow production best practices—this is a learning environment
3. **Platform targeting**: When working with Flutter, confirm target platform before building (web vs native)
4. **Windows environment**: Commands should use PowerShell-compatible syntax
5. **No backend**: React projects are frontend-only, no API or database integrations

## Common Tasks

### Starting a new experiment
- Create a new directory at repository root
- Add a README.md explaining the experiment's purpose
- Include appropriate .gitignore for the language/framework
- Update this file if the experiment introduces new patterns

### Testing changes
- React: Changes auto-reload via Vite HMR
- Flutter: Use hot reload (r) for UI changes, hot restart (R) for state/initialization changes
- TypeScript: Recompile with `tsc` or use `ts-node --watch`
- Ada: Must recompile with gnatmake

### Dependencies
- React: Check [package.json](Hemp-app/package.json) - uses React 19.x+ with latest Vite
- Flutter: Check [pubspec.yaml](hello_pi/pubspec.yaml) - currently minimal dependencies (cupertino_icons only)
- Python: Check [requirements.txt](hemp-django/requirements.txt) - pygame only

## File Locations Quick Reference

- **Main READMEs**: Each project has its own README.md in the project root
- **Entry points**: 
  - React: `src/main.tsx` → `src/App.tsx`
  - Flutter: `lib/main.dart`
  - Python: `hemp-danjo.py`
  - Ada: `*.adb` files
- **Config files**: 
  - TypeScript: `tsconfig.json`, `tsconfig.app.json`, `tsconfig.node.json`
  - Flutter: `pubspec.yaml`, `analysis_options.yaml`
  - React/Vite: `vite.config.ts`, `eslint.config.js`
