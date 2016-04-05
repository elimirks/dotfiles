#!/bin/bash
sleep 10
conky -c ~/.conky/conkyclo &
sleep 5
conky -c ~/.conky/conkyrcmx &
sleep 5
exec conky -c ~/.conky/conkynet
