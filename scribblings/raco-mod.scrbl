#lang scribble/manual
@(require (for-label racket/base pkg/name))

@(define (cmd . s) (apply exec s))
@(define (row . cells) cells)
@(define (table . rows)
   (tabular #:style 'boxed #:sep @hspace[2] #:row-properties '(bottom-border ()) rows))

@title[#:tag "raco-mod"]{raco mod: Per-Project Dependencies}
@author["dannypsnl"]

@exec{raco mod} gives each project its own dependencies, resolved with Go's @hyperlink["https://research.swtch.com/vgo-mvs"]{Minimal Version Selection} (MVS) and kept out of the global installation. @filepath{info.rkt} is still the manifest format, therefore, the same package still installs with plain @exec{raco pkg}.

@commandline["raco mod get megaparsack-lib@latest"]

@table-of-contents[]

@; ------------------------------------------------------------------------
@section[#:tag "commands"]{Commands}

@table[
 @row[@bold{Command} @bold{Effect}]
 @row[@cmd{sync}
      @elem{resolve @filepath{info.rkt} deps with MVS, write @filepath{info.sum},
            and link the build list into @filepath{.rkt/}}]
 @row[@cmd{get pkg[@"@"X.Y|@"@"latest|@"@"none] ...}
      @elem{set @racket[#:version] in the @racket[deps] of @filepath{info.rkt}
            (or remove the dependency), re-pin, then sync}]
 @row[@cmd{list} @elem{print the build list}]
 @row[@cmd{graph} @elem{print the requirement graph}]
 @row[@cmd{why pkg} @elem{show the requirement chain that selects @exec{pkg}'s version}]
 @row[@cmd{env} @elem{print this project's environment variables (fish or sh)}]
 @row[@cmd{exec cmd args ...} @elem{sync, then run @exec{cmd} in the project's environment}]
 @row[@cmd{clean [-n]}
      @elem{delete cached packages, downloads, mirrors, and compiled code that
            no project uses (@exec{-n}: only list them)}]]

@exec{get} rewrites only the @racket[deps] form of @filepath{info.rkt};
comments and every other definition stay as they were. When another dependency
forces a higher version than the one asked for, @exec{get} says so and shows
the chain from @exec{why}.

@; ------------------------------------------------------------------------
@section[#:tag "project"]{The Project}

A project is an ordinary @exec{raco pkg} package directory: the nearest
directory with @filepath{info.sum}, else the nearest with @filepath{info.rkt}.
Its own collection is linked following @exec{raco pkg}'s rules (a
@racket[collection] string, @racket['multi], or the directory name), so the
project's modules can require each other inside @exec{exec}.

@table[
 @row[@bold{Concept} @bold{Comes from}]
 @row[@elem{requirements}
      @elem{@racket[deps], plus @racket[build-deps] for the project itself}]
 @row[@elem{versions} @elem{the @racket[version] field of each package's @filepath{info.rkt}}]
 @row[@elem{local overrides}
      @elem{@racket[(define mod-replace '(("pkg" "../path")))] in the project's
            @filepath{info.rkt}; the package takes the version its directory
            declares and need not be in any catalog}]
 @row[@elem{standard library}
      @elem{packages in installation scope: not selected by MVS, but their
            @racket[#:version] floors are checked}]]

@filepath{info.sum} is generated: one @tt{pkg version id time h1:hash} line
per package, where the id is a commit, @tt{sha1:}@italic{archive-hash}, or
@tt{dir:}@italic{content-hash}, and the time is when that content was made.
It makes builds reproducible and verifies content. Commit it.

@exec{get} edits @filepath{info.rkt} and @filepath{info.sum} together: if
anything fails, both are left as they were.

@; ------------------------------------------------------------------------
@section[#:tag "versions"]{Choosing Versions}

MVS selects, for every package, the maximum of the minimum versions required
anywhere in the graph. What a "minimum" is matters in an ecosystem where most
packages write @racket["tzinfo"] rather than @racket[("tzinfo" #:version "0.6")]:

@itemlist[
 @item{A dependency's minimum is its @racket[#:version], @bold{raised to the
       version that was current when the depending content was made} (its
       commit time). An author who wrote an unversioned dependency built
       against what was current then; the oldest version is almost never what
       they meant.}
 @item{The project's own unversioned dependencies keep the version
       @filepath{info.sum} pinned, or else take the latest known one;
       @exec{sync} warns about them, and @exec{raco mod get pkg@"@"latest}
       records one.}
 @item{The time of each package's content is pinned in @filepath{info.sum}
       with the content itself, so the same @filepath{info.rkt} and
       @filepath{info.sum} select the same build list on every machine, even
       after upstream moves on.}]

@exec{get pkg@"@"latest} re-pins: if the content behind a version moved on
upstream (the newest commit of the current version, a republished archive, a
changed directory), @exec{get} switches to it, while @exec{sync} keeps what
@filepath{info.sum} pinned.

Catalog entries are read as @exec{raco pkg} reads them: an entry in
@racket[versions] for the running Racket version wins over the default
@racket[source]. Source types are inferred exactly like @exec{raco pkg} does
(@racket[package-source->name+type]), so a non-GitHub URL not ending in
@filepath{.git} is a remote directory, not a git repository.

@subsection{Git Sources}

@itemlist[
 @item{Versions are the history of @racket[version] in @filepath{info.rkt} on
       the default branch; no tags are needed.}
 @item{Version V is the @bold{last} commit while the version is still V. A
       superseded version is therefore fixed forever; the current one is
       pinned by @filepath{info.sum}.}]

@subsection{Archives}

@filepath{.zip}, @filepath{.tgz}, and @filepath{.plt} archives, such as the
Rhombus packages:

@itemlist[
 @item{The version is the @racket[version] in the archive's
       @filepath{info.rkt}; the id is the archive's SHA1. An unchanged catalog
       checksum means no download, and a download that does not match the
       catalog's checksum is refused, as @exec{raco pkg} does.}
 @item{An archive URL only serves its current content, so older versions
       survive only in the local cache. If one is needed, missing from the
       cache, and gone upstream, @exec{raco mod} says so and suggests
       @exec{get pkg@"@"latest}.}
 @item{Entries that would escape the destination (@filepath{..}, absolute
       paths) are refused. @filepath{.plt} archives are only unpacked; their
       config and setup unit are never evaluated.}
 @item{Built packages ship machine-independent @filepath{.zo} files; the first
       @exec{raco make} turns them into native code, the same step
       @exec{raco setup} performs.}]

@subsection{Directory Sources}

@itemlist[
 @item{Remote directories follow @exec{raco pkg}: fetch
       @filepath{MANIFEST}, then every file it lists.}
 @item{Local directories are copied; a local directory that is a git
       repository is used as one.}
 @item{The id is a content hash. Like archives, only the current content is
       served.}]

@; ------------------------------------------------------------------------
@section[#:tag "cache"]{Isolation and the Cache}

@verbatim|{
~/.cache/raco-mod/
  pkg/<pkg>@<ver>+<id>/   extracted packages, read-only, shared by all projects
  zo/<key>/               compiled code, shared by all projects
  vcs/ dl/ index/ source/ git mirrors, downloads, version indexes, catalog entries
  projects.rktd           projects that use the cache, for `clean`
<project>/.rkt/           (gitignored)
  addon/<version>/links.rktd
  compiled/
}|

@exec{exec}, and the variables printed by @exec{env}, set:

@itemlist[
 @item{@envvar{PLTADDONDIR} to @filepath{<project>/.rkt/addon}, so the build
       list is the user-scope links file and globally installed user-scope
       packages are invisible.}
 @item{@envvar{PLTCOMPILEDROOTS} to @filepath{<project>/.rkt/compiled:}, so
       nothing is written into the shared cache and the installation's own
       @filepath{.zo} files are reused. The empty trailing element is the
       symbol @racket['same]; a literal @tt{same} would be a relative directory
       named @filepath{same}.}]

Compiled code is shared: @filepath{<project>/.rkt/compiled/<package path>} is
a symbolic link to @filepath{zo/<key>}, where the key covers the package's
content, the keys of all its dependencies, and the Racket installation.
Projects that select the same versions compile each package once (Rhombus:
29 seconds in the first project, 1.3 seconds in the second). The compilation
manager still validates every @filepath{.zo} by SHA1, so sharing cannot make a
build wrong. Not shared: @racket[mod-replace]d packages, anything depending on
them, and dependency cycles.

@exec{raco mod clean} removes what no registered project uses. Every
@exec{sync} registers its project in @filepath{projects.rktd}; @exec{clean}
keeps the @filepath{zo/} directories those projects link to and the packages
their @filepath{info.sum} names, together with those packages' archives and
git mirrors, deletes the rest, and forgets projects that no longer exist.
Removed archive and directory versions also leave their version index, so
they are never selected again; removed git versions stay, since their mirror
can be cloned again.

@exec{clean} never runs on its own. An archive's older versions exist only in
this cache, so deleting one cannot be undone; use @exec{raco mod clean -n} to
see what would go first.

@; ------------------------------------------------------------------------
@section[#:tag "limitations"]{Limitations}

@itemlist[
 @item{Version granularity is whatever authors declare: a package that never
       bumps @racket[version] (i.e. always @tt{0.0}) has a
       single version.}
 @item{One version per package in a build, matching Racket's collection model.}
 @item{No Go-style downgrade: asking for a version below another dependency's
       floor selects the higher version and explains why, without downgrading
       others.}
 @item{@exec{clean} only knows projects that ran @exec{sync} on this
       machine: a checkout that has not synced yet re-fetches what it needs,
       and an archive version that is gone upstream can no longer be fetched.}]
