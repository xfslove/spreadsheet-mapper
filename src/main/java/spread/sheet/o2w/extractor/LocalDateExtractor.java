package spread.sheet.o2w.extractor;

import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.o2w.composer.WorkbookComposeException;
import spread.sheet.utils.FieldUtils;

/**
 * local date text value with supplied pattern extractor
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateExtractor extends FieldValueExtractorAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateExtractor.class);

  private String pattern;

  public LocalDateExtractor(String pattern, String matchField) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public String getStringValue(Object data, FieldMeta fieldMeta) {

    try {
      Object value = PropertyUtils.getProperty(data, FieldUtils.detectRealField(fieldMeta.getName()));

      if (!(value instanceof LocalDate)) {
        return null;
      }

      return ((LocalDate) value).toString(pattern);

    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }
}
