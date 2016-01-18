from sqlalchemy import *
from sqlalchemy.orm import *
from sqlalchemy.databases.mysql import MSBigInteger, MSTimeStamp

from sqlalchemy.orm import mapper

metadata = MetaData()

t_user = Table('user', metadata,
    Column('user_id', Integer, primary_key=True),
    Column('token', String(16), unique=True),
    Column('ts', MSTimeStamp, nullable=False),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_profile = Table('profile', metadata,
    Column('profile_id', Integer, primary_key=True),
    Column('user_id', Integer, ForeignKey('user.user_id'), nullable=False),
    Column('origin', Integer, nullable=False),
    Column('reference', Unicode(32)),
    Column('data', String(1024), nullable=False),
    Column('user_data', String(1024)),
    Column('ts', MSTimeStamp, nullable=False),
    UniqueConstraint('origin', 'reference'),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_topic = Table('topic', metadata,
    Column('topic_id', Integer, primary_key=True),
    Column('user_id', Integer, ForeignKey('user.user_id'), nullable=True),
    Column('profile_id', Integer, ForeignKey('profile.profile_id'), nullable=True),
    Column('name', Unicode(32), nullable=True),
    UniqueConstraint('user_id', 'profile_id', 'name'),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_message = Table('message', metadata,
    Column('message_id', Integer, primary_key=True),
    Column('topic_id', Integer, ForeignKey('topic.topic_id')),
    Column('origin', Integer, nullable=False),
    Column('profile', Integer, nullable=False),
    Column('author', String(64), nullable=False),
    Column('author_id', String(64), nullable=False),
    Column('forwarded_by', String(64), nullable=True),
    Column('reference', String(64)),
    Column('irt_reference', String(64)),
    Column('irt_user', String(64)),
    Column('irt_user_id', String(64)),
    Column('posted', Integer, nullable=False),
    Column('body', Unicode(160)),
    Column('attached', Integer, nullable=False, default=0),
    Column('favorited', Boolean, nullable=False, default=False),
    Column('ts', MSTimeStamp, nullable=False),
    UniqueConstraint('topic_id', 'origin', 'reference'),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_related = Table('related', metadata,
    Column('message_id', Integer, ForeignKey('message.message_id'), nullable=False),
    Column('origin', Integer, nullable=False),
    Column('reference', String(64), nullable=False),
    UniqueConstraint('origin', 'reference'),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_related2 = Table('related2', metadata,
    Column('origin1', Integer, nullable=False),
    Column('reference1', String(64), nullable=False),
    Column('origin2', Integer, nullable=False),
    Column('reference2', String(64), nullable=False),
    UniqueConstraint('origin1', 'reference1'),
    UniqueConstraint('origin2', 'reference2'),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_bt_device = Table('bt_device', metadata,
    Column('device_id', MSBigInteger, nullable=False),
    Column('address', String(12), nullable=False),
    Column('major', Integer, nullable=False),
    Column('minor', Integer, nullable=False),
    Column('services', Integer, nullable=False),
    Column('friendly', String(250), nullable=True),
    Column('ts', Integer),
    UniqueConstraint('device_id', 'address', 'major', 'minor', 'services', 'friendly'),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

t_bt_encounter = Table('bt_encounter', metadata,
    Column('local', MSBigInteger, nullable=False),
    Column('remote', MSBigInteger, nullable=False),
    Column('ts', Integer, nullable=False),
    Column('duration', Integer, nullable=False),
    mysql_charset='utf8',
    mysql_engine='InnoDB'
)

class User(object): pass
class Profile(object): pass
class Topic(object): pass
class Message(object): pass

mapper(Profile, t_profile)

mapper(User, t_user, properties={
    'profiles': relation(Profile),
    'topics': relation(Topic),
})

mapper(Topic, t_topic, properties={
    'profile': relation(Profile),
    'user': relation(User),
})

mapper(Message, t_message)
