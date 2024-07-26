usethis::create_from_github(
  # github link
  "https://github.com/frasansa/shortage-viz.git",
  # destdir
  destdir = file.path(".."))

#if private repo run in terminal at R_projects level:
#git clone https://github.com/frasansa/shortage-viz.git

# Regenerate token
# click the email link
# copy the ghp code
# go to the dispositive --> gitcreds::gitcreds_set() and paste the code
