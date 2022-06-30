import React from 'react';
import { Link } from 'react-router-dom';
import mobiles from './../../assets/mobiles.jpeg';
import computers from './../../assets/computers.jpeg';
import camera from './../../assets/camera.jpeg';
import printer from './../../assets/printer.jpeg';
import headPhone from './../../assets/headPhone.jpeg';
import television from './../../assets/television.jpeg';





import './styles.scss';

const Directory = props => {
  return (
    <div className="directory">
      <div className="wrap">
        <div
          className="item"
          style={{
            backgroundImage: `url(${computers})`
          }}
        >
          <Link to="/search/Computer">
            Shop Computers
          </Link>
        </div>
        <div
          className="item"
          style={{
            backgroundImage: `url(${mobiles})`
          }}
        >
          <Link to="/search/Mobile">
            Shop Mobiles
          </Link>
        </div>
        <div
          className="item"
          style={{
            backgroundImage: `url(${camera})`
          }}
        >
          <Link to="/search/Camera">
            Shop cameras
          </Link>
        </div>
        <div
          className="item"
          style={{
            backgroundImage: `url(${printer})`
          }}
        >
          <Link to="/search/Printer">
            Shop Printers
          </Link>
        </div>
        <div
          className="item"
          style={{
            backgroundImage: `url(${headPhone})`
          }}
        >
          <Link to="/search/headPhone">
            Shop headPhone
          </Link>
        </div>
        <div
          className="item"
          style={{
            backgroundImage: `url(${television})`
          }}
        >
          <Link to="/search/Television">
            Shop Televisions
          </Link>
        </div>
      </div>


    </div>

  );
};

export default Directory;
