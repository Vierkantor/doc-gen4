/-
Copyright (c) 2025 Anne Baanen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen
-/
import DocGen4.Output.Module

namespace SubDocGen

open scoped DocGen4.Jsx
open DocGen4 Lean

def SupplementSection.renderDocstrings
    (sec : SupplementSection MarkdownDocstring) :
    Output.HtmlM (SupplementSection Html) := do
  return { sec with text := <p>[(← Output.docStringToHtml sec.text sec.name)]</p> }

def SupplementSectionEntry.renderDocstrings
    (sec : SupplementSectionEntry MarkdownDocstring) :
    Output.HtmlM (SupplementSectionEntry Html) := do
  return { sec with text := <p>[(← Output.docStringToHtml sec.text sec.name)]</p> }

def SupplementPageEntry.renderDocstrings
    (page : SupplementPageEntry MarkdownDocstring) :
    Output.HtmlM (SupplementPageEntry Html) := do
  return { page with sections := ← page.sections.mapM (· |>.renderDocstrings) }

end SubDocGen

namespace DocGen4.Output

open scoped DocGen4.Jsx
open Lean SubDocGen

/--
Render the HTML for a single section in a supplemental page.
-/
def sectionToHtml (sec : SupplementSection Html) : Html :=
  <div id={sec.name}>
    <h2>{sec.name}</h2>
    {sec.text}
    <dl>[
      if let some mod := sec.definingModule then
        #[<dt>Defined in module:</dt>, <dd>{mod.toString}</dd>]
      else
        #[]
    ]</dl>
  </div>

def sectionNavLink (sec : SupplementSection Html) : Html :=
  <p><a href={"#".append sec.name}>{sec.name}</a></p>

/--
Render the HTML for an entire supplemental page.
-/
def supplementPageToHtml (page : SupplementPage Html) : BaseHtmlM Html := do
  let sectionsHtml := page.sections.map sectionToHtml
  templateLiftExtends (baseHtmlGenerator page.name) <| pure #[
    <nav class="internal_nav">
      <p><a href="#top">return to top</a></p>
      [page.sections.map sectionNavLink]
    </nav>,
    Html.element "main" false #[] (#[page.intro] ++ sectionsHtml)
  ]

/-- Parts of the supplement that do not come from declarations in a specific module.

This can be, for example, built-in syntax.
-/
def builtinSupplement : Std.HashMap String (SupplementPage Html) := .ofList [
  ("Commands", {
    name := "Commands",
    intro := <p>Commands provide a way to interact with and modify a Lean environment outside of the context of a proof. Familiar commands from core Lean include <code>#check</code>, <code>#eval</code>, and <code>run_cmd</code>.</p>,
    sections := #[],
  }),
  ("Tactics", {
    name := "Tactics",
    intro := <p>The tactic language is a special-purpose programming language for constructing proofs, indicated using the <code>by</code> keyword.</p>,
    sections := #[],
  }),
]

open IO in
/-- Read the generated supplementary pages from a folder containing JSON files. -/
def loadSupplementJSON (filePath : System.FilePath) : IO (Array (SupplementPage Html)) := do
  -- Fetch the declared pages, which might still have missing sections.
  -- We will fill in those later.
  let mut pages : Std.HashMap String (SupplementPage Html) := builtinSupplement
  for entry in ← System.FilePath.readDir filePath do
    if entry.fileName.startsWith "pages-" && entry.fileName.endsWith ".json" then
      let fileContent ← FS.readFile entry.path
      match Json.parse fileContent with
      | .error err =>
        throw <| IO.userError s!"failed to parse file '{entry.path}' as json: {err}"
      | .ok jsonContent =>
        match fromJson? jsonContent with
        | .error err =>
          throw <| IO.userError s!"failed to parse file '{entry.path}': {err}"
        | .ok (arr : Array (SupplementPageEntry Html)) =>
          for entry in arr do
            pages := pages.insert entry.key entry.toSupplementPage

  -- Fill in sections declared in downstream files.
  for entry in ← System.FilePath.readDir filePath do
    if entry.fileName.startsWith "sections-" && entry.fileName.endsWith ".json" then
      let fileContent ← FS.readFile entry.path
      match Json.parse fileContent with
      | .error err =>
        throw <| IO.userError s!"failed to parse file '{entry.path}' as json: {err}"
      | .ok jsonContent =>
        match fromJson? jsonContent with
        | .error err =>
          throw <| IO.userError s!"failed to parse file '{entry.path}': {err}"
        | .ok (arr : Array (SupplementSectionEntry Html)) =>
          for entry in arr do
            pages := pages.modify entry.pageKey fun page =>
              { page with sections := page.sections.push entry.toSupplementSection }

  return pages.values.toArray

/-- Save supplementary pages declared in a specific module.

This `abbrev` exists as a type-checking wrapper around `toJson`, ensuring `loadSupplementJSON` gets
objects in the expected format.
-/
abbrev saveSupplementPageJSON (fileName : System.FilePath) (pages : Array (SupplementPageEntry Html)) : IO Unit :=
  IO.FS.writeFile fileName (toString (toJson pages))

/-- Save sections of supplementary pages declared in a specific module.

This `abbrev` exists as a type-checking wrapper around `toJson`, ensuring `loadSupplementJSON` gets
objects in the expected format.
-/
abbrev saveSupplementSectionJSON (fileName : System.FilePath) (sections : Array (SupplementSectionEntry Html)) : IO Unit := do
  IO.FS.writeFile fileName (toString (toJson sections))

end Output
end DocGen4
