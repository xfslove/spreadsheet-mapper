package spread.sheet.o2w.extractor;

import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.o2w.composer.WorkbookComposeException;
import spread.sheet.utils.FieldUtils;

import java.math.BigDecimal;

/**
 * plain number text value extractor
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public class PlainNumberExtractor extends FieldValueExtractorAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(PlainNumberExtractor.class);

  public PlainNumberExtractor(String matchField) {
    super(matchField);
  }

  @Override
  public String getStringValue(Object data, FieldMeta fieldMeta) {

    try {
      Object value = PropertyUtils.getProperty(data, FieldUtils.detectRealField(fieldMeta.getName()));

      if (!(value instanceof Number)) {
        return null;
      }

      if (value instanceof BigDecimal) {
        return ((BigDecimal) value).stripTrailingZeros().toPlainString();
      } else if (value instanceof Double) {
        return BigDecimal.valueOf((Double) value).stripTrailingZeros().toPlainString();
      } else if (value instanceof Float) {
        return new BigDecimal(Float.toString((Float) value)).stripTrailingZeros().toPlainString();
      }
      return value.toString();

    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }
}
