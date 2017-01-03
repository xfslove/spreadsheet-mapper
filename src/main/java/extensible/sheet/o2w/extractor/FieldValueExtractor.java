package extensible.sheet.o2w.extractor;

/**
 * field value extract to human readable value
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface FieldValueExtractor extends ValueExtractor {

  /**
   * @return which field this extractor matched
   */
  String getMatchField();
}
