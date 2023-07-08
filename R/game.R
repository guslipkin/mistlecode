#' #' Create an AoC Assembly Computer
#' #'
#' #' @rdname game
#' #' @md
#' #'
#' #' @param players A list of registers
#' #' @param spells A list of functions. Registers can be referred to with
#' #'   `self[[register_name]]`. They must have two arguments, `x` and `y`, even if
#' #'   only one is used. The index is iterated with `private$.inc(j)` where `j` is
#' #'   the number of places to "jump." Example: `\(x, y) private$.inc(self[[x]])`
#' #'   will increment `self$index` by `self[[x]]`.
#' #' @param regex (Default: `"[, \\+]+"`) The pattern to use for
#' #'   [stringr::str_split]
#' #'
#' #' @details
#' #' `try_numeric(x)`
#' #' :  Takes input and tries to cast it as numeric. If that fails, it returns the
#' #'    original object.
#' #'
#' #' `val_or_index(x)`
#' #' :  If `x` is numeric, return `x`, otherwise return `self[[x]]`.
#' #'
#' #' `call(f, x, y)`
#' #' :  Calls one of `functions` using `x` and `y` as arguments.
#' #'    There are two ways the function can be used:
#' #'    * `f` is not length one and `x` and `y` are null, then `f[1](f[2], f[3])`
#' #'    * `f` is length one, and `x` and `y` are not null, then `f(x, y)`
#' #'
#' #' `run(x, target, until, pattern)`
#' #' :  Tries to run the instructions provided by `x`. If `x` is a character
#' #'    vector, it will try to split the instructions into a segments for
#' #'    `call(f, x, y)`.
#' #'    * `x`: A character vector of instructions as `c("f x y")` or a list of
#' #'      character vectors in the format `list(c("f", "x", "y"))`
#' #'    * `target`: (Default: `NULL`) The `register` to print as the final result.
#' #'      If `NULL`, `self` will be printed.
#' #'    * `pattern`: See the `regex` argument for more details.
#' #'
#' #' `.inc(increment)`
#' #' :  A private function used to increment `self$index` by `increment`.
#' #'
#' #' @returns A new `assembly` R6 object with `registers` and `functions` as the
#' #'   fields and methods, respectively. Along with the functions detailed in the
#' #'   details section.
#' #' @export
#' create_game <- function(players, spells, effects, regex = "[, \\+]+") {
#'   tmp_players <-
#'     players |>
#'     `names<-`(glue::glue("tmp_{names(players)}"))
#'   game <-
#'     R6::R6Class(
#'       "game",
#'       public = unlist(list(
#'         as.list(c(players, spells, effects)),
#'         "turn" = 1,
#'         "try_numeric" = \(x) {
#'           xx <- suppressWarnings(as.numeric(x))
#'           if (is.na(xx)) x else xx
#'         },
#'         "val_or_index" = \(x) { if (is.numeric(x)) x else self[[x]] },
#'         "cast" = \(s, attacker, defender) {
#'
#'         },
#'         "play" = \(x, target = NULL, until = length(x), pattern = regex) {
#'           if (!is.list(x)) { x <- stringr::str_split(x, pattern) }
#'           while(self$index <= until) {
#'             self$call(x[[self$index]])
#'           }
#'           if (is.null(target)) self else self[[target]]
#'         }
#'       ), recursive = FALSE),
#'       private = unlist(list(
#'         as.list(c(tmp_players)),
#'         "durations" = as.list(rlang::rep_named(names(spells), 0)),
#'         "do_damage" = \(attacker, defender) {
#'
#'         }
#'       ), recursive = FALSE)
#'     )
#'   return(game$new())
#' }
#'
#' create_player <- function(hp, damage = 0, armor = 0, mana = Inf) {
#'   player <-
#'     R6::R6Class(
#'       "player",
#'       public = list(
#'         "hp" = \(value) {
#'           if (missing(value)) { private$.hp }
#'           else { private$.hp <- private$.hp + value; self; }
#'         },
#'         "damage" = \(value) {
#'           if (missing(value)) { private$.damage }
#'           else { private$.damage <- private$.base_damage + value; self; }
#'         },
#'         "armor" = \(value) {
#'           if (missing(value)) { private$.armor }
#'           else { private$.armor <- private$.base_armor + value; self; }
#'         },
#'         "mana" = \(value) {
#'           if (missing(value)) { private$.mana }
#'           else { private$.mana<- private$.mana + value; self; }
#'         },
#'         "effects" = list(),
#'         "add_effect" = \(name, duration) {
#'           if (is.null(self$effects[[name]])) {
#'             self$effects[[name]] <- duration
#'           }
#'           return(self)
#'         },
#'         "consume_effect" = \(name) {
#'           self$effects[[name]] <- self$effects[[name]] - 1
#'           if (self$effects[[name]] == 0) { self$effects[[name]] <- NULL }
#'           return(self)
#'         }
#'       ),
#'       private = list(
#'         ".hp" = hp,
#'         ".damage" = damage,
#'         ".armor" = armor,
#'         ".mana" = mana,
#'         ".base_damage" = damage,
#'         ".base_armor" = armor
#'       )
#'     )
#'   return(player$new())
#' }
#'
#' create_spell <- function(type = c("instant", "effect"), cost, ...) {
#'   argList <- rlang::list2(...)
#'   duration <- argList$duration
#'   argList$duration <- NULL
#'   names(argList) <- glue::glue(".{names(argList)}")
#'   instant <-
#'     R6::R6Class(
#'       "instant",
#'       public = list(
#'         "cost" = cost,
#'         "cast" = \(defender) {
#'
#'         },
#'         "hp" = \(who, hp) { who$hp(hp) },
#'         "damage" = \(who, damage) {
#'           damage <- damage - who$armor()
#'           damage <- ifelse(damage < 1, 1, damage)
#'           who$damage(damage)
#'         },
#'         "armor" = \(who, armor) {
#'           armor <- who$armor(armor)
#'           armor <- ifelse(armor < 0, 0, armor)
#'           who$armor(armor)
#'         },
#'         "mana" = \(who, mana) { who$mana(mana) }
#'       ),
#'       private = unlist(list(
#'         c(argList),
#'         "modifies" = list(names(argList))
#'       ), recursive = FALSE)
#'     )
#'
#'   effect <-
#'     R6::R6Class(
#'       "effect",
#'       inherit = instant,
#'       public = list(
#'         "cast" = \(attacker, defender) {
#'           # private$modifies |>
#'           # stringr::str_extract("[A-z]+") |>
#'           # purrr::walk(\(.x) {
#'           #   self[[.x]](attacker, self[[glue::glue(".{.x}")]])
#'           # })
#'           f <- stringr::str_extract(private$modifies, "[A-z]+")
#'           i <- 1
#'           while (i <= length(f)) {
#'             print(f[i])
#'             self[[f[i]]](attacker, self[[private$modifies[i]]])
#'             i <- i + 1
#'           }
#'           attacker
#'           # attackerself[["hp"]](attacker, 5)
#'         },
#'         "duration" = duration
#'       )
#'     )
#'
#'   if (type == "instant") {
#'     argList$duration <- NULL
#'     return(instant$new())
#'   } else if (type == "effect" & !is.null(duration)) {
#'     return(effect$new())
#'   } else {
#'     return(NULL)
#'   }
#' }
#'
#'
#' # create_spell("instant", 10, damage = 5, hp = 2) -> s
#' create_spell("effect", 10, duration = 6, damage = 2, hp = 2) -> s
#' create_player(50, 1, 0, 500) -> p
#' s$cast(p)
#'
#' p$damage()
#' p$damage(5)
#' p$mana(-500)
#' p$add_effect("lightning", 5)
#' p$effects
#' p$consume_effect("lightning")
#' s
#' create_game(list("a" = create_player(50, 0, 0, 500), "b" = list("hp" = 3)), list("c" = 3))
