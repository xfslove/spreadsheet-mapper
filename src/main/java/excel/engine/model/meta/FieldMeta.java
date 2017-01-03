package excel.engine.model.meta;

import java.io.Serializable;
import java.util.List;

/**
 * <pre>
 * field meta include:
 * 1. field name (this is importance, is determined all cell value of the same column index corresponding which field of object)
 * 2. field at column index
 * 3. list of header meta of this field (see {@link HeaderMeta})
 * </pre>
 * Created by hanwen on 2016/12/30.
 */
public interface FieldMeta extends Serializable {

  /**
   * @return field name
   */
  String getName();

  /**
   * which column
   *
   * @return 1-based
   */
  int getColumnIndex();

  /**
   * @return list of header meta
   */
  List<HeaderMeta> getHeaderMetas();

  /**
   * get header at row index
   *
   * @param rowIndex 1-based
   * @return header meta
   */
  HeaderMeta getHeaderMeta(int rowIndex);

  /**
   * add header meta
   *
   * @param headerMeta header meta
   */
  void addHeaderMeta(HeaderMeta headerMeta);

  /**
   * @return the sheet meta of this
   */
  SheetMeta getSheetMeta();
}
