import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';
import React, { useEffect, useState } from 'react';

import './styles.scss';


export const firestore = firebase.firestore();
export const auth = firebase.auth();
const EditProfile = props => {
    const [firstName, setFirstName] = useState('');
    const [lastName, setLastName] = useState('');
    const [mobileNumber, setMobileNumber] = useState('');
    useEffect(async () => {
        getUser()

    }, []);


    const getUser = async () => {
        const userRef = firestore.doc(`users/${auth.currentUser.uid}`);
        const snapshot = await userRef.get();
        console.log(snapshot?.data()['firstName'])
        setFirstName(snapshot?.data()['firstName'])
        setLastName(snapshot?.data()['lastName'])
        setMobileNumber(snapshot?.data()['mobileNumber'])

    }


    const handleFirstName = event => {
        setFirstName(event.target.value)
    };

    const handleLastName = event => {
        setLastName(event.target.value)
    };
    const handleMobileNumber = event => {
        setMobileNumber(event.target.value)
    };

    const updateProfile = async () => {
        var data = {
            firstName: firstName,
            lastName: lastName,
            fullName: firstName + ' ' + lastName,
            mobileNumber: mobileNumber,
            id: auth.currentUser.uid,
            userName: auth.currentUser.displayName,
            email: auth.currentUser.email,
            timestampUpdate: Date.now()
        };


        const userRef = firestore.doc(`users/${auth.currentUser.uid}`);
        const snapshot = await userRef.get();

        if (snapshot.exists) {
            try {
                // auth.currentUser.updatePhoneNumber(mobileNumber)
                auth.currentUser.updateProfile({ displayName: firstName + ' ' + lastName })
                await userRef.update({
                    firstName: firstName,
                    lastName: lastName,
                    fullName: firstName + ' ' + lastName,
                    mobileNumber: mobileNumber,
                    id: auth.currentUser.uid,
                    userName: auth.currentUser.displayName,
                    email: auth.currentUser.email,
                    timestampUpdate: Date.now()
                });
                window.location.reload(true);

            } catch (err) {
                console.log(err);
            }
        }

    };

    return (
        <div className="homepage">
            <h1>
                My profile
        </h1>
            <div className="formWrap">
                <ul>
                    <li>
                        <input
                            type="text"
                            value={firstName}
                            name="firstName"
                            placeholder="FirstName"
                            onChange={handleFirstName}
                        />

                    </li>
                    <li>

                        <input
                            type="text"
                            value={lastName ?? ''}
                            name="lastName"
                            placeholder="LastName"
                            onChange={handleLastName}
                        />
                    </li>
                    <li>

                        <input
                            type="text"
                            value={mobileNumber ?? ''}
                            name="firstName"
                            placeholder="Mobile Number"
                            onChange={handleMobileNumber}
                        />
                    </li>
                    <li>
                        <button type="submit" onClick={updateProfile}>Submit</button>

                    </li>
                </ul>


            </div>




        </div>
    );
};

export default EditProfile;