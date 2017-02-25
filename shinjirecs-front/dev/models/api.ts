export interface ReservationI {
  id: number;
  title: string;
}

export interface API {
  get_reservations(): ReservationI[]
  onNetworkError : () => void;
  onTimeoutError : () => void;
  get_reservations(): ReservationI[]
  get_reservation(id: number): ReservationI
  post_reservation(record: ReservationI): ReservationI
  patch_reservation(record: ReservationI): ReservationI
  delete_reservation(id: number): Boolean
}
