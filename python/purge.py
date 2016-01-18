#!/usr/bin/env python
import os
import logging
import sys

from ConfigParser import ConfigParser

from sqlalchemy import engine_from_config
from sqlalchemy.orm import create_session
from sqlalchemy.orm import sessionmaker
from sqlalchemy.sql import select, and_
from sqlalchemy.sql import func

import db
from db import User, Topic

def start(conf):
    # connect to db
    db.engine = engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    db.metadata.bind = engine
    conn = engine.connect()

    Session = sessionmaker(bind=engine)
    session = Session()
    
    profiles = []
    topics = []
    for user in session.query(User):
        for profile in user.profiles:
            if profile.origin == 5:
                profiles.append(profile.profile_id)
        for topic in user.topics:
            if topic.profile_id in profiles:
                topics.append(topic.topic_id)

    for topic_id in topics:
        print "checking", topic_id
        s = select([func.count(db.t_message.c.message_id)], and_(db.t_message.c.origin == 5, db.t_message.c.topic_id == topic_id))
        (count,) = conn.execute(s).fetchone()
        if count > 1000:
            (m_id,) = conn.execute(select([db.t_message.c.message_id],
                                           db.t_message.c.topic_id == topic_id).order_by(
                                                    db.t_message.c.message_id.desc()).offset(1000).limit(1)).fetchone()
            print "purging", topic_id, count, m_id
            conn.execute(db.t_message.delete().where(and_(db.t_message.c.message_id < m_id, db.t_message.c.topic_id == topic_id)))

if __name__ == '__main__':
    conf = ConfigParser()
    conf.read('config')
    start(conf)
