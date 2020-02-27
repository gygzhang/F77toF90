sdir="savedir6"
fns=`ls ${sdir}`

echo ${fns}

for fn in ${fns};do
    sed -i "s/\r//g" ${sdir}/${fn}
done
