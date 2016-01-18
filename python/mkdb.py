#!/usr/bin/env python
import os
import logging
import sys

from ConfigParser import ConfigParser

from sqlalchemy import engine_from_config
from sqlalchemy.orm import create_session

import db
def start(conf):
    # connect to db
    db.engine = engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    db.metadata.bind = engine
    db.metadata.drop_all()
    db.metadata.create_all()

if __name__ == '__main__':
    conf = ConfigParser()
    conf.read('config')
    start(conf)
