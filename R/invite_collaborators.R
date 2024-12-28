#' Invite collaborators to a GitHub repository
#'
#' This function invites one or more collaborators to a GitHub repository, specifying the level of access.
#'
#' @param repo_name The name of the GitHub repository.
#' @param collaborators A character vector of GitHub usernames to be added as collaborators.
#' @param permission The permission level for the collaborators: 'pull', 'push', or 'admin'.
#' @return Invisible NULL, primarily used for the side effects of adding collaborators.
#' @export
#' @examples
#' invite_collaborators("mynewrepo", c("user1", "user2"), "push")
#' @importFrom gh gh
invite_collaborators <- function(repo_name, collaborators, permission = "push") {
  # Check if repository name and collaborators are provided
  if (!nzchar(repo_name)) {
    stop("Invalid repository name: must be a non-empty string.")
  }
  if (length(collaborators) == 0) {
    stop("No collaborators specified.")
  }
  if (!permission %in% c("pull", "push", "admin")) {
    stop("Invalid permission level. Choose from 'pull', 'push', or 'admin'.")
  }

  # Attempt to add each collaborator
  owner <- gh::gh_whoami()$login
  for (collab in collaborators) {
    tryCatch({
      gh::gh(
        "PUT /repos/:owner/:repo/collaborators/:username",
        owner = owner,
        repo = repo_name,
        username = collab,
        permission = permission
      )
      message(paste("Successfully added collaborator:", collab))
    }, error = function(e) {
      message(paste("Failed to add collaborator:", collab, "Error:", e$message))
    })
  }

  invisible(NULL)
}
