export interface ReservationI {
  id: number;
  title: string;
}

export interface API {
  get_reservations(): ReservationI[]
  onNetworkError : () => void;
  onTimeoutError : () => void;
}
