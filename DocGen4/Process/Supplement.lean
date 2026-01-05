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
import SubDocGen

namespace SubDocGen.SupplementPage

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
