# geostools 0.2.0

Changes to narrow the dependencies of **geotools**:

* Function `MinimizeSpherical()` is copied from package **pfields** in order to
  be able to use the full functionality of the function `GetDistance()` without
  the need to have a suggest dependence on **pfields** (in turn, the function
  can be removed from **pfields** to have **pfields** depend on
  **geotools**). This also offers a clearer thematic distinction between the two
  packages.
* There is no need to import base package **Matrix** into the NAMESPACE, a
  simple package dependence is enough.
  
In addition, some improvements on function documentation are added.

# geostools 0.1.0

* Initial package version with number and scope of the functions incl. their
  documentation identical to their original versions in deprecated package
  'ecustools' which they were a part of.
