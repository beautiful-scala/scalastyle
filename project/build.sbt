val scalastyleVersion = settingKey[String]("Scalastyle version")
scalastyleVersion := sys.props.getOrElse("scalastyle.version", "1.2.0")

libraryDependencies += "com.beautiful-scala" %% "scalastyle" % scalastyleVersion.value
