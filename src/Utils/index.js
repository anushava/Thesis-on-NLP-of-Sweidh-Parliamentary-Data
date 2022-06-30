import axios from 'axios';

export const checkUserIsAdmin = currentUser => {
  if (!currentUser || !Array.isArray(currentUser.userRoles)) return false;
  const { userRoles } = currentUser;
  if (userRoles.includes('admin')) return true;

  return false;
}

export const apiInstance = axios.create({
  headers: {
    'Allow-Control-Allow-Origin': '*',
    'Content-Type': 'application/json'
  },
  baseURL: 'https://us-central1-shoppingwebsite-f3066.cloudfunctions.net/api'
});