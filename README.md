# HivepodR
R Package for manipulating data created with [Hivepod](https://www.hivepod.io) backends.

## Sample Backend
See [https://jacaton-r.herokuapp.com](https://jacaton-r.herokuapp.com/#/oficina/) as a sample Hivepod produced backend to test the package.
Use the credentials: demo / 1234 as a readonly access.

## Usage

```
library(HivepodR)
install_github("SevillaR/hivepodR")

cnx <- connect("http://jacaton-r.herokuapp.com", "demo", "1234") 
off <- resource(cnx, "oficinas") 

count(off)
query(off)
query(off, conditions=buildCondition("nombre", "==", "Seville")  )
query(off, limit=2, skip=2)

```


## Initial Authors

- Irene Mendoza
- Elena Goméz-Diaz [elegodi](https://github.com/elegodi)
- Ignasi Bartomeus [ibartomeus](https://github.com/ibartomeus)
- Pedro J. Molina [pjmolina](https://github.com/pjmolina)