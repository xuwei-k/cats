classDiagramSettings

val urlMapping: PartialFunction[Class[_], String] = {
  case clazz if clazz.getName.startsWith("cats") =>
    s"https://github.com/non/cats/tree/master/core/src/main/scala/${clazz.getName.replace('.', '/')}.scala"
}

DiagramKeys.classDiagramSetting ~= { s =>
  s.copy(
    nodeSetting = clazz => s.nodeSetting(clazz) ++ urlMapping.lift(clazz).map("href" -> _).toList,
    commonNodeSetting = s.commonNodeSetting + ("target" -> "_blank")
  )
}

