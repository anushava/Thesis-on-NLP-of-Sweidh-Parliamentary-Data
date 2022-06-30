import React from 'react';
import './styles.scss';
import userIMG from './../../assets/user.png';
import { Link, useLocation } from 'react-router-dom';
import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';

export const firestore = firebase.firestore();
export const auth = firebase.auth();
const UserProfile = props => {
  const { currentUser } = props;
  const { displayName } = currentUser;

  const sendPasswordEmailReset = async () => {
    auth.sendPasswordResetEmail(auth.currentUser.email)
  }

  return (
    <div className="userProfile">
      <ul>
        <li>
          <div className="img">
            <img src={userIMG} />
          </div>
        </li>
        <li>
          <span className="displayName">
            {displayName && displayName}
          </span>
          <span className="displayName">
            <Link to="/profile">
              Edit Profile
              </Link>
          </span>
          <span className="displayName">
            <Link onClick={sendPasswordEmailReset} >
              Reset Password
              </Link>
          </span>
        </li>


      </ul>
    </div>
  );
}

export default UserProfile;