## Kmeans in sml

#### Build compiler

```
cd compiler
make clean && make
cd -
```
#### Examples 

+ Sum of a list of int, no appr.

```
./run.sh source_code/data.sml source_code/foldl.sml
```

+ Sum of a list of int, appr.

```
./run.sh source_code/data.sml source_code/foldl_appr.sml
```

+ Kmeans, appr.

```
./run.sh source_code/kmeans_data.sml source_code/kmeans.sml
```
