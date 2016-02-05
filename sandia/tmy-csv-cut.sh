#!/bin/bash

IN=$1
OUT=$2

csvcut -c "local time","air temp max","air temp min","air temp mean","dew point max","dew point min","dew point mean","wind speed max","wind speed mean","ghi mean","dni mean" ${IN} > ${OUT}
