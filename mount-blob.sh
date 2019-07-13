if [ ! -d "/mnt/blobfusetemp" ] 
then
    sudo mkdir /mnt/blobgfusetemp
    sudo chown mattcoop /mnt/blobfusetemp
fi

blobfuse /home/mattcoop/mortalityblob --tmp-path=/mnt/blobfusetemp  --config-file=/home/mattcoop/.fuse_connection.cfg -o attr_timeout=240 -o entry_timeout=240 -o negative_timeout=120 -o allow_other
