#!/usr/bin/env python
import os
import logging
import sys

from ConfigParser import ConfigParser

from sqlalchemy import engine_from_config
from sqlalchemy.orm import create_session
from sqlalchemy.orm import sessionmaker
from sqlalchemy.sql import text

import db
from db import User, Topic

def start(conf):
    # connect to db
    db.engine = engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    db.metadata.bind = engine

    Session = sessionmaker(bind=engine)
    session = Session()
    
# don't add favorites, using transient topic for it
#    for user in session.query(User):
#        for profile in user.profiles:
#            if profile.origin == 5:
#                topic = Topic()
#                topic.user_id = user.user_id
#                topic.profile_id = profile.profile_id
#                topic.name = 'favorites'
#                session.add(topic)
#                session.flush()
#    session.commit()
    
if __name__ == '__main__':
    conf = ConfigParser()
    conf.read('config')
    start(conf)
