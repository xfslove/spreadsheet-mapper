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
   * @param data supplied object
   * @return human readable value
   */
  String getStringValue(Object data);

  /**
   * matches which field
   *
   * @param field field
   * @return success
   */
  boolean matches(String field);
}
