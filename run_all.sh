#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# 2020
roc test 2020/01.roc
roc 2020/01.roc < input/2020_01.txt

# 2021
roc test 2021/01.roc
roc 2021/01.roc < input/2021_01.txt

roc test 2021/02.roc
roc 2021/02.roc < input/2021_02.txt

roc test 2021/03.roc
roc 2021/03.roc < input/2021_03.txt

# 2022
roc test 2022/01.roc
roc 2022/01.roc < input/2022_01.txt

roc test 2022/02.roc
roc 2022/02.roc < input/2022_02.txt

# 2023
roc test 2023/01.roc
roc 2023/01.roc < input/2023_01.txt

# roc test 2023/02.roc
roc 2023/02.roc < input/2023_02.txt

roc test 2023/03.roc
roc 2023/03.roc < input/2023_03.txt

roc test 2023/04.roc
roc 2023/04.roc < input/2023_04.txt

roc test 2023/05.roc
roc 2023/05.roc < input/2023_05.txt

roc test 2023/09.roc
roc 2023/09.roc < input/2023_09.txt
