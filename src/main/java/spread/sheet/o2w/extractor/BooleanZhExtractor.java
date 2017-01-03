package spread.sheet.o2w.extractor;


import spread.sheet.model.meta.FieldMeta;
import spread.sheet.o2w.composer.WorkbookComposeException;
import spread.sheet.util.FieldUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * boolean readable value extractor
 * <p>
 * Created by hanwen on 16/3/18.
 */
public class BooleanZhExtractor extends FieldValueExtractorAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanZhExtractor.class);

  private String trueString;

  private String falseString;

  public BooleanZhExtractor(String matchField, String trueString, String falseString) {
    super(matchField);
    this.trueString = trueString;
    this.falseString = falseString;
  }

  @Override
  public String getStringValue(Object data, FieldMeta fieldMeta) {

    try {
      Object value = PropertyUtils.getProperty(data, FieldUtils.detectRealField(fieldMeta.getName()));

      if (Boolean.FALSE.equals(value)) {
        return falseString;
      } else if (Boolean.TRUE.equals(value)) {
        return trueString;
      }
      return null;
    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }
}
