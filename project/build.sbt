val scalastyleVersion = settingKey[String]("Scalastyle version")
scalastyleVersion := IO.read(new File("project/scalastyle-version"))

libraryDependencies += "com.beautiful-scala" %% "scalastyle" % scalastyleVersion.value
