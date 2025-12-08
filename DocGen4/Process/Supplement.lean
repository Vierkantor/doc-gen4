/-
Copyright (c) 2025 Anne Baanen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen
-/

import Lean.Data.Json.FromToJson.Basic
import Lean.Data.Json.Parser
import Lean.DocString
import Lean.Meta.Basic
import Lean.Parser.Extension

namespace DocGen4.Process

open Lean Meta

/-- A docstring formatted in Markdown. -/
abbrev MarkdownDocstring := String

/-- A section out of a `SupplementPage`. -/
structure SupplementSection (textFormat : Type) where
  /-- Human-readable title of the section. -/
  name : String

  /-- Main contents of this section. -/
  text : textFormat

  /-- Module where this section is defined. -/
  definingModule : Option Name
  /-- Declaration(s) relating to this section. 

  For example, a tactic might link here to its implementation.
  -/
  relatedDecls : Array Name
deriving FromJson, ToJson

/--
A page to be rendered in addition to the module docs.

Pages have some introductory text, followed by headered sections.
-/
structure SupplementPage (textFormat : Type) where
  /-- Human-readable name of the page. -/
  name : String
  /-- Page introduction. -/
  intro : textFormat
  /-- Sections of the page. -/
  sections : Array (SupplementSection textFormat)

-- TODO: declare an environment extension here so projects can declare their own pages.

namespace SupplementPage

/-- Return a filename, without extension, corresponding to the human-readable name. -/
def baseFileName (page : SupplementPage textFormat) : String :=
  -- For now, just replace all non-alphabetical characters with `_`s.
  page.name.foldl (init := "") fun acc c =>
    if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then acc.push c
    else acc.push '_'

/-- Return the filename, with extension, for this page rendered to HTML. -/
def fileName (page : SupplementPage textFormat) : String :=
  page.baseFileName.append ".html"

end SupplementPage

/-- A section out of a `SupplementPage`, plus information on where to place the section. -/
structure SupplementSectionEntry (textFormat) extends SupplementSection textFormat where
  /-- Identifies the page to which this section belongs. -/
  pageKey : String
deriving FromJson, ToJson

/-- A page to be rendered in addition to the module docs, plus information on
where to find its sections. -/
structure SupplementPageEntry (textFormat) extends SupplementPage textFormat where
  /-- A unique identifier for this page, used to associate sections declared in downstream modules. -/
  key : String
deriving FromJson, ToJson

end DocGen4.Process
