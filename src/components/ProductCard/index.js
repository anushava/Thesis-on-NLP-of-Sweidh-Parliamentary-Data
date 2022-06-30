import React, { useEffect, useState } from 'react';
import { useParams, useHistory } from 'react-router-dom';
import { useDispatch, useSelector } from 'react-redux';
import { fetchProductStart, setProduct } from './../../redux/Products/products.actions';
import { addProduct } from './../../redux/Cart/cart.actions';
import Button from './../forms/Button';
import firebase from 'firebase/app';
import 'firebase/firestore';
import 'firebase/auth';
import './styles.scss';
import ReactStars from "react-rating-stars-component";
export const firestore = firebase.firestore();
export const auth = firebase.auth();
const mapState = state => ({
  product: state.productsData.product
});
const ProductCard = ({ }) => {
  const dispatch = useDispatch();
  const history = useHistory();
  const { productID } = useParams();
  const { product } = useSelector(mapState);
  const [rating, setRating] = useState('');
  const [review, serReview] = useState('');



  const {
    productThumbnail,
    productName,
    productPrice,
    productDesc,
    isDiscount,
    discountValue,
    reviews
  } = product;

  useEffect(() => {
    dispatch(
      fetchProductStart(productID)
    )
    return () => {
      dispatch(
        setProduct({})
      )
    }


  }, []);




  const handleAddToCart = (product) => {
    if (!product) return;
    dispatch(
      addProduct(product)
    );
    history.push('/cart');
  }

  const configAddToCartBtn = {
    type: 'button'
  }



  const handleInputChange = event => {
    serReview(event.target.value)
  };


  const addRating = async () => {
    var data = {
      id: Math.round((new Date()).getTime() / 1000),
      userId: auth.currentUser.uid,
      userName: auth.currentUser.displayName,
      userEmail: auth.currentUser.email,
      description: review,
      rating: rating,
      timestampCreated: Date.now()
    };


    const productsRef = firestore.doc(`products/${productID}`);
    const snapshot = await productsRef.get();

    if (snapshot.exists) {

      reviews?.push(data)
      console.log(reviews)
      try {
        await productsRef.update({
          'reviews': reviews ? reviews : [data]
        });
        window.location.reload(true);

      } catch (err) {
        console.log(err);
      }
    }

  };

  return (
    <div className="productCard">
      <div className="hero">
        <img src={productThumbnail} />
      </div>
      <div className="productDetails">
        <ul>
          <li>
            <h1>
              {productName}
            </h1>
          </li>
          <li>
            <span>
              SEK {isDiscount == 'Yes' ? productPrice - discountValue : productPrice} {isDiscount == 'Yes' ? <div className="discount">{'Discount upto ' + discountValue} SEK </div> : ''}
            </span>
          </li>
          <li>
            <div className="addToCart">
              <Button {...configAddToCartBtn} onClick={() => handleAddToCart(product)}>
                Add to cart
              </Button>
            </div>
          </li>
          <li>
            <span
              className="desc"
              dangerouslySetInnerHTML={{ __html: productDesc }} />
          </li>



          {
            reviews != null && (

              <ul>
                <h3>Reviews</h3>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[0]?.userName }} />

                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[0]?.description }} />
                </li>
                <li>
                  {reviews[0] != null && <ReactStars
                    count={5}
                    value={reviews[0]?.rating}
                    edit={false}
                    size={24}
                    activeColor="#ffd700"
                  />}
                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[1]?.userName }} />

                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[1]?.description }} />
                </li>
                <li>
                  {reviews[1] != null && <ReactStars
                    count={5}
                    value={reviews[1]?.rating}
                    edit={false}
                    size={24}
                    activeColor="#ffd700"
                  />}
                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[2]?.userName }} />

                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[2]?.description }} />
                </li>
                <li>
                  {reviews[2] != null && <ReactStars
                    count={5}
                    value={reviews[2]?.rating}
                    edit={false}
                    size={24}
                    activeColor="#ffd700"
                  />}
                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[3]?.userName }} />

                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[3]?.description }} />
                </li>
                <li>
                  {reviews[3] != null && <ReactStars
                    count={5}
                    value={reviews[3]?.rating}
                    edit={false}
                    size={24}
                    activeColor="#ffd700"
                  />}
                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[4]?.userName }} />

                </li>
                <li>
                  <span
                    className="desc"
                    dangerouslySetInnerHTML={{ __html: reviews[4]?.description }} />
                </li>
                <li>
                  {reviews[4] != null && <ReactStars
                    count={5}
                    value={reviews[4]?.rating}
                    edit={false}
                    size={24}
                    activeColor="#ffd700"
                  />}
                </li>
              </ul>
            )
          }


          <input
            type="text"
            name="review"
            placeholder="Review"
            onChange={handleInputChange}
          />
          <ReactStars
            count={5}
            size={24}
            activeColor="#ffd700"
            onChange={setRating}
          />
          <button type="submit" onClick={addRating}>Submit</button>

        </ul>
      </div>
    </div>
  );
}

export default ProductCard;
