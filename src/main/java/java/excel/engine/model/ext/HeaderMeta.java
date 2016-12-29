package java.excel.engine.model.ext;

import java.io.Serializable;

/**
 * <pre>
 * sheet header meta, default supplied three headers
 * 1. {@link #META_FIELD}
 * 2. {@link #META_TITLE}
 * 3. {@link #META_PROMPT}
 * </pre>
 * Created by hanwen on 2016/12/29.
 */
public interface HeaderMeta extends Serializable {

  /**
   * <pre>
   * field
   * when read required field row, this is importance when read excel, is determined cell value corresponding which field of object
   * when write may empty
   * </pre>
   */
  String META_FIELD = "header_meta_field";

  /**
   * title
   */
  String META_TITLE = "header_meta_title";

  /**
   * prompt
   */
  String META_PROMPT = "header_meta_prompt";

  /**
   * header at which row
   *
   * @return 1-based
   */
  int getRowIndex();

  /**
   * @return header name
   */
  String getName();
}
