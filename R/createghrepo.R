#' Create a new GitHub repository and set an upstream
#'
#' This function initializes a new GitHub repository with the specified name,
#' renames the local master branch to main, sets a remote named upstream,
#' and pushes the main branch to the newly set upstream.
#'
#' @inheritParams invite_collaborators
#' @param upstream The URL of the upstream repository to set for pushing changes.
#' @return Invisible NULL, used for side effects of creating and setting up the repository.
#' @export
#' @examples \dontrun{
#' create_ghrepo("mynewrepo", "https://github.com/username/mynewrepo.git")
#' }

create_ghrepo <- function(projname, upstream, collaborators = character()) {
  # Check if the projname is a non-empty string
  if (!is.character(projname) || nzchar(projname) == FALSE) {
    stop("Invalid project name: must be a non-empty string.")
  }

  # Check if the upstream URL is valid (accept both HTTPS and SSH)
  if (!is.character(upstream) || (grepl("^https?://", upstream) == FALSE && grepl("^ssh://", upstream) == FALSE && !grepl("^git@", upstream))) {
    stop("Invalid upstream URL: must be a valid HTTPS or SSH URL.")
  }

  # Check if the master branch exists and attempt to rename it
  branches <- system("git branch", intern = TRUE)  # Capture output of git branch
  if ("master" %in% branches) {
    if (system("git branch -m master main") != 0) {
      stop("Failed to rename the branch from master to main.")
    }
  }

  # Create repository on GitHub using the gh package
  message("Creating GitHub repository...")
  tryCatch({
    repo <- gh::gh(
      "POST /user/repos",
      name = projname,
      private = TRUE,
      auto_init = FALSE
    )
  }, error = function(e) {
    stop("Failed to create GitHub repository: ", e$message)
  })

  # Set and verify the upstream remote
  message("Setting upstream repository...")
  if (system(paste0("git remote add upstream ", upstream)) != 0 ||
      system("git remote -v") != 0) {
    stop("Failed to set upstream repository.")
  }

  # Push the main branch to the newly created upstream
  message("Pushing to upstream repository...")
  if (system("git push --set-upstream upstream main") != 0) {
    stop("Failed to push local branch main to upstream.")
  }

  # Invite collaborators if specified
  if (length(collaborators) > 0) {
    invite_collaborators(projname, collaborators)
  }

  invisible(NULL)
}
