
// do not include all repositories in the POM
// (this is important for staging since artifacts published to a staging repository
//  can be promoted (i.e. published) to another repository)
pomAllRepositories := false

// make sure no repositories show up in the POM file
pomIncludeRepository := { _ => false }

// include *.zip and *.tgz artifacts in the POM dependency section
makePomConfiguration :=
  makePomConfiguration.value.copy(includeTypes = Set(Artifact.DefaultType, Artifact.PomType, "zip", "tgz"))

// publish Maven POM metadata (instead of Ivy);
// this is important for the UpdatesPlugin's ability to find available updates.
publishMavenStyle := true

// publish to bintray.com via: `sbt publish`
publishTo := Some(
  "TIWG" at
    s"https://api.bintray.com/content/tiwg/org.omg.tiwg/${moduleName.value}/${version.value}")

PgpKeys.useGpg := true

PgpKeys.useGpgAgent := true

pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

versionWithGit
