import React, {Component, PropTypes} from 'react';
import { connect } from 'react-redux';
import { sendMsg } from '../actions/chat';
import styles from './ChatRoom.css';
import CSSModules from 'react-css-modules';

@CSSModules(styles)
class ChatRoom extends Component {
    render() {
        const { dispatch, currentRoom, roomMap } = this.props;
        let messages = [];

        if(currentRoom) {
            messages = roomMap[currentRoom].messages;
            return (
                <div styleName="room-content">
                    <h1>{currentRoom}</h1>
                    <ul ref="msglist">
                        {messages.map(function(msg, idx){
                            switch(msg.type) {
                                case 'join':
                                    return <li key={idx} styleName="event-msg">{msg.username} JOINED {msg.roomname}</li>;
                                case 'leave':
                                    return <li key={idx} styleName="event-msg">{msg.username} LEAVED {msg.roomname}</li>;
                                case 'msg':
                                    return (<li key={idx} styleName="normal-msg">
                                            <span styleName="msg-sender">{msg.username}</span>
                                            <span styleName="msg-content">{msg.payload}</span>
                                            </li>);
                                default:
                                    break
                            }
                        })}
                    </ul>
                    <form styleName="room-action">
                        <input type="text" ref="sendBox" />
                        <button styleName="room-action-button"
                                onClick={(e) => {e.preventDefault();
                                                 this.send()}
                                }>Send</button>
                    </form>
                </div>
            );
        } else {
            return <div>Select A room or create one to chat</div>;
        }
    }

    componentDidUpdate() {
        let ul = this.refs.msglist;
        if(ul != undefined){
            ul.scrollTop = ul.scrollHeight;
        }
    }

    send() {
        const { dispatch, currentRoom } = this.props;
        const textNode = this.refs.sendBox;
        let text = (textNode.value || '').trim();
        textNode.value = '';

        if (currentRoom == null || !text) {
            return;
        }
        dispatch(sendMsg(currentRoom, text));
    };
}


ChatRoom.propTypes = {
};


function select(state) {
    return {
        currentRoom: state.chat.currentRoom,
        roomMap: state.chat.roomMap
    };
}


export default connect(select)(ChatRoom);
