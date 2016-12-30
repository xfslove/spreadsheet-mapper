package excel.engine.model.meta;

import java.io.Serializable;

/**
 * <pre>
 * header meta include:
 * row index
 * text value
 * </pre>
 * Created by hanwen on 2016/12/29.
 */
public interface HeaderMeta extends Serializable, Comparable<HeaderMeta> {

  /**
   * header at which row
   *
   * @return 1-based
   */
  int getRowIndex();

  /**
   * @return header value
   */
  String getValue();

  /**
   * @return the field meta meta of this
   */
  FieldMeta getFieldMeta();
}
