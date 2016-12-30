package excel.engine.model.meta;

import java.io.Serializable;
import java.util.List;

/**
 * <pre>
 * the field meta include:
 * field name
 * field at column index
 * list of header meta of this field (see {@link HeaderMeta})
 * </pre>
 * Created by hanwen on 2016/12/30.
 */
public interface FieldMeta extends Serializable, Comparable<FieldMeta> {

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
   * @return header metas sort by {@link HeaderMeta#getRowIndex()}
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
