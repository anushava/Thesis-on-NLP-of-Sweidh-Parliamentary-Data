import React, { useEffect, useState } from 'react';
import {
  TableContainer, Table, TableHead,
  TableBody, TableRow, TableCell
} from '@material-ui/core';
import { useDispatch } from 'react-redux';
import { setOrderDetails } from './../../redux/Orders/orders.actions';
import FormInput from './../forms/FormInput';
import Button from './../forms/Button';

const columns = [
  {
    id: 'productThumbnail',
    label: ''
  },
  {
    id: 'productName',
    label: 'Name'
  },
  {
    id: 'productPrice',
    label: 'Price'
  },
  {
    id: 'quantity',
    label: 'Quantity'
  },

]

const styles = {
  fontSize: '16px',
  width: '10%'
};

const formatText = (columnName, columnValue) => {
  switch (columnName) {
    case 'productPrice':
      return `SEK${columnValue}`;
    case 'productThumbnail':
      return <img src={columnValue} width={250} />;
    default:
      return columnValue;
  }
}

const OrderDetails = ({ order }) => {
  const dispatch = useDispatch();
  const orderItems = order && order.orderItems;
  const [review, setReview] = useState('');


  useEffect(() => {
    return () => {
      dispatch(
        setOrderDetails({})
      );
    }
  }, []);


  const handleSubmit = e => {

  }


  return (
    <TableContainer>
      <Table>

        <TableHead>
          <TableRow>

            {columns.map((col, pos) => {
              return (
                <TableCell
                  key={pos}
                  style={styles}
                >
                  {col.label}
                </TableCell>

              )
            })}

          </TableRow>
        </TableHead>

        <TableBody>

          {(Array.isArray(orderItems) && orderItems.length > 0) && orderItems.map((row, pos) => {
            return (
              <TableRow key={pos}>

                {columns.map((col, pos) => {
                  const columnName = col.id;
                  const columnValue = row[columnName];

                  return (
                    <TableCell
                      key={pos}
                      style={styles}
                    >
                      {formatText(columnName, columnValue)}
                    </TableCell>

                  )
                })}
                <form onSubmit={handleSubmit}>

                  <FormInput
                    type="review"
                    name="review"
                    value={review}
                    placeholder="Review"
                    handleChange={e => setReview(e.target.value)}
                  />

                  <Button type="submit">
                    Submit Review
                  </Button>

                </form>


              </TableRow>
            )
          })}

        </TableBody>

      </Table>
    </TableContainer>
  )
}

export default OrderDetails;
