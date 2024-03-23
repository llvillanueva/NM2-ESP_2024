#!/usr/bin/env python3
import json
import os
homedir = os.getenv('HOME')
with open(os.path.join(homedir,
          '.local/share/jupyter/kernels/iris/kernel.json'),'r') as f:
    config = json.load(f)
config["argv"][0] = os.path.join(homedir,'.conda/envs/iris/bin/python3')
with open(os.path.join(homedir,
          '.local/share/jupyter/kernels/iris/kernel.json'),'w') as f:
    f.write(json.dumps(config,indent=1))
