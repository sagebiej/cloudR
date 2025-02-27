#' Create a new GitHub repository and set an upstream
#'
#' This function initializes a new GitHub repository with the specified name,
#' renames the local master branch to main, sets a remote named upstream,
#' and pushes the main branch to the newly set upstream.
#'
#' @inheritParams invite_collaborators
#' @param projname The name of the project. It must be the same as in the upstream link.
#' @param upstream The URL of the upstream repository to set for pushing changes.
#' @param repo_type The type of repository to create. Can be `"personal"` (default) or `"organization"`.
#' @param org_name The organization name (required if `repo_type = "organization"`).
#' @param collaborators A character vector of GitHub usernames to be added as collaborators.
#' @return Invisible NULL, used for side effects of creating and setting up the repository.
#' @export
#' @examples \dontrun{
#' create_ghrepo("mynewrepo", "https://github.com/username/mynewrepo.git", repo_type = "personal")
#' create_ghrepo("orgrepo", "https://github.com/myorg/orgrepo.git", repo_type = "organization", org_name = "myorg")
#' }

create_ghrepo <- function(projname, upstream, repo_type = "personal", org_name = NULL, collaborators = character()) {
  # Validate projname
  if (!is.character(projname) || nzchar(projname) == FALSE) {
    stop("Invalid project name: must be a non-empty string.")
  }

  # Validate upstream URL (HTTPS or SSH)
  if (!is.character(upstream) ||
      (!grepl("^https?://", upstream) && !grepl("^git@", upstream))) {
    stop("Invalid upstream URL: must be a valid HTTPS or SSH URL.")
  }

  # Ensure valid repo_type
  if (!repo_type %in% c("personal", "organization")) {
    stop("Invalid repo_type: must be 'personal' or 'organization'.")
  }

  # Ensure org_name is provided if creating an organization repository
  if (repo_type == "organization" && (is.null(org_name) || nzchar(org_name) == FALSE)) {
    stop("Organization name must be provided when repo_type is 'organization'.")
  }

  # Rename 'master' to 'main' if it exists
  branches <- system("git branch", intern = TRUE)  # Get list of branches
  if ("master" %in% branches) {
    if (system("git branch -m master main") != 0) {
      stop("Failed to rename the branch from master to main.")
    }
  }

  # Create repository using gh::gh
  message("Creating GitHub repository...")

  repo_endpoint <- switch(repo_type,
                          "personal" = "POST /user/repos",
                          "organization" = paste0("POST /orgs/", org_name, "/repos"))

  tryCatch({
    repo <- gh::gh(
      repo_endpoint,
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
