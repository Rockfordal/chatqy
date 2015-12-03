import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { pushState } from 'redux-router';
import { getRoomList, initConnect, enterRoom } from '../actions/chat';
import RoomOp from './RoomOp';
import ChatRoom from './ChatRoom';

class ChatRooms extends Component {
    render() {
        const { roomMap } = this.props;

        const lst = [];
        for (let p in roomMap) {
            lst.push({name: p, description: roomMap[p].description})
        }

        return (
            <div className="pure-g">
              <div className="pure-u-3-5">
                  <ChatRoom />
              </div>
              <div className="pure-u-2-5">
                <RoomOp />
                <div>TODO</div>
              </div>
            </div>
        );
    }

    handleClick(roomname) {
        const { dispatch } = this.props;
        dispatch(enterRoom(roomname));
    }

    componentWillMount() {
        const {dispatch} = this.props;
        dispatch(getRoomList());
        dispatch(initConnect());
    }
}


ChatRooms.propTypes = {
    roomMap: PropTypes.object
};


function select(state) {
    return {
        roomMap: state.chat.roomMap
    };
}

export default connect(select)(ChatRooms);
