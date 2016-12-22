package me.excel.tools.extractor;

/**
 * field value extract to human readable value
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface FieldValueExtractor {

  /**
   * get human readable value to shown on cell
   *
   * @param data
   * @return
   */
  String getStringValue(Object data);

  /**
   * matches which field
   *
   * @param field
   * @return
   */
  boolean matches(String field);
}
