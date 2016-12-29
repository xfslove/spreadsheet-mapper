package java.excel.engine.model.ext;

import java.io.Serializable;

/**
 * sheet header, sheet template see {@link SheetTemplate}
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface SheetHeader extends Serializable {

  /**
   * @return which header
   * @see HeaderMeta
   */
  HeaderMeta getHeaderMeta();

  /**
   * add value to this header
   *
   * @param field field
   * @param value value
   */
  void addValue(String field, String value);

  /**
   * get header value of field
   *
   * @param field field
   * @return header value
   */
  String getValue(String field);
}
