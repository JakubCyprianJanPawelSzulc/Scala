
import org.scalatest.funsuite.AnyFunSuite

class Test2 extends AnyFunSuite {
  val indexList = wynik
  val indexMap = indexList.toMap
  val ojczyznyList = List(396, 654, 700, 765)
  val żyćList = List(355, 732, 754, 1302, 1451, 1591, 1696, 2283)
  val zList = List(40, 41, 45, 54, 60, 65, 66, 76, 77, 82, 86, 98, 107, 108, 133, 136, 153, 157,
    159, 164, 165, 166, 168, 171, 187, 189, 190, 201, 208, 212, 223, 235, 237, 258, 261, 262, 263,
    278, 282, 283, 285, 296, 297, 305, 306, 311, 320, 322, 323, 353, 354, 377, 386, 398, 405, 407,
    413, 415, 417, 418, 438, 440, 453, 454, 457, 460, 477, 479, 480, 485, 491, 503, 506, 530, 533,
    551, 555, 563, 574, 580, 581, 588, 603, 632, 634, 637, 648, 650, 657, 660, 663, 664, 665, 667,
    671, 673, 684, 685, 688, 697, 699, 706, 717, 718, 728, 739, 741, 743, 744, 772, 774, 779, 780,
    781, 786, 797, 807, 810, 822, 833, 866, 867, 886, 893, 932, 939, 945, 949, 957, 967, 968, 976,
    991, 996, 1040, 1047, 1053, 1063, 1077, 1078, 1084, 1098, 1109, 1124, 1125, 1126, 1158, 1161,
    1175, 1181, 1185, 1204, 1205, 1217, 1218, 1236, 1237, 1239, 1241, 1252, 1254, 1257, 1263, 1266,
    1267, 1268, 1270, 1299, 1313, 1325, 1340, 1353, 1357, 1366, 1374, 1385, 1387, 1388, 1389, 1391,
    1392, 1401, 1402, 1423, 1427, 1438, 1451, 1454, 1457, 1460, 1461, 1463, 1464, 1467, 1468, 1469,
    1472, 1475, 1541, 1596, 1599, 1608, 1609, 1618, 1620, 1626, 1628, 1637, 1640, 1642, 1645, 1650,
    1657, 1658, 1669, 1679, 1688, 1690, 1698, 1715, 1726, 1734, 1746, 1758, 1761, 1769, 1771, 1781,
    1785, 1796, 1799, 1813, 1819, 1828, 1829, 1842, 1848, 1850, 1864, 1865, 1876, 1885, 1891, 1896,
    1897, 1898, 1899, 1901, 1902, 1905, 1912, 1913, 1917, 1938, 1944, 1973, 1985, 1991, 1996, 2003,
    2006, 2011, 2014, 2017, 2018, 2020, 2023, 2026, 2027, 2029, 2030, 2041, 2048, 2057, 2069, 2077,
    2102, 2106, 2114, 2118, 2133, 2136, 2137, 2138, 2157, 2158, 2164, 2174, 2189, 2201, 2213, 2216,
    2217, 2221, 2226, 2236, 2243, 2244, 2245, 2247, 2248, 2249, 2253, 2263, 2270, 2273, 2296, 2305,
    2317, 2339, 2351, 2362, 2364, 2368)

  test("Number of indexed words") {
    assert(indexList.length === 6103)
  }

  test("Number of lines containing the word “książę”") {
    assert(indexMap.get("książę").getOrElse(Nil).length === 118)
  }

  test("Number of lines containing the word “ojczyzny”") {
    assert(indexMap.get("ojczyzny").getOrElse(Nil).length === 4)
  }

  test("Number of lines containing the word “państwo”") {
    assert(indexMap.get("państwo").getOrElse(Nil).length === 40)
  }

  test("Number of lines containing the word “obywateli”") {
    assert(indexMap.get("obywateli").getOrElse(Nil).length === 19)
  }

  test("Lines containing the word “ojczyzny”") {
    assert(indexMap.get("ojczyzny").getOrElse(Nil) === ojczyznyList)
  }

  test("Lines containing the word “żyć”") {
    assert(indexMap.get("żyć").getOrElse(Nil) === żyćList)
  }

  test("Lines containing the word “z”") {
    assert(indexMap.get("z").getOrElse(Nil) === zList)
  }

}
