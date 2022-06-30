import React, { useState, useEffect } from 'react';
import { Link, useLocation } from 'react-router-dom';
import { useSelector, useDispatch } from 'react-redux';
import { signOutUserStart } from './../../redux/User/user.actions';
import { selectCartItemsCount } from './../../redux/Cart/cart.selectors';
import './styles.scss';

import Logo from './../../assets/logo.png';
import Footer from '../Footer';

const mapState = (state) => ({
  currentUser: state.user.currentUser,
  totalNumCartItems: selectCartItemsCount(state)
});

const Header = props => {
  const location = useLocation();
  const [activeMenu, setActiveMenu] = useState(false);
  const dispatch = useDispatch();
  const { currentUser, totalNumCartItems } = useSelector(mapState);

  const signOut = () => {
    dispatch(signOutUserStart());
  };

  useEffect(() => {
    setActiveMenu(false);
  }, [location]);

  return (
    <header className="header">
      <div className="wrap">
        <div className="logo">
          <Link to="/">
            <img src={Logo} alt="Shopping24SevenLOGO" />
          </Link>
        </div>

        <nav className={`mainMenuactive ${activeMenu ? 'active' : ''}`}>
          <ul>
            <li>
              <Link to="/">
                Home
              </Link>
            </li>
            <li>
              <Link to="/search">
                Search
              </Link>
            </li>
            <li>
              <Link to="/search">
                About us
              </Link>
            </li>
            <li>
              <Link to="/search">
                Contact Us
              </Link>
            </li>
          </ul>
        </nav>

        <div className="callToActions">

          <ul>

            <li>
              <Link to="/cart">
                Your Cart ({totalNumCartItems})
                <i class="fas fa-shopping-basket"></i>
              </Link>
            </li>

            {currentUser && [
              <li key={1}>
                <Link to="/dashboard">
                  My Account
                  <i class="fas fa-user-circle"></i>
                </Link>
              </li>,
              <li key={2}>
                <span onClick={() => signOut()}>
                  LogOut
                  <i class="fas fa-sign-out-alt"></i>
                </span>
              </li>
            ]}

            {!currentUser && [
              <li key={1} className="hideOnMobile">
                <Link to="/registration">
                  Register
                </Link>
              </li>,
              <li key={2}>
                <Link to="/login">
                  Login
                  <i class="fas fa-user-circle"></i>
                </Link>
              </li>
            ]}

            <li className="mobileMenu">
              <span onClick={() => setActiveMenu(!activeMenu)}>
                <i className="fas fa-bars"></i>
              </span>
            </li>

          </ul>




        </div>
      </div>
    </header>
  );
};

Header.defaultProps = {
  currentUser: null
};

export default Header;
