package excel.engine.exporter.extractor;

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
   * @return which field this extractor matched
   */
  String getMatchField();

  /**
   * @return which sheet this extractor matched
   */
  int getSheetIndex();
}
