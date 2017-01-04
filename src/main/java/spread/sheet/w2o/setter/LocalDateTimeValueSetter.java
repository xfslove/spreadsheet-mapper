package spread.sheet.w2o.setter;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.utils.FieldUtils;
import spread.sheet.w2o.processor.WorkbookProcessException;

/**
 * local date time field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateTimeValueSetter.class);

  private String pattern;

  public LocalDateTimeValueSetter(String pattern, String matchField) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public void set(Object data, Cell cell, FieldMeta fieldMeta) {
    try {
      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);
      String value = cell.getValue();
      String fieldName = FieldUtils.detectRealField(fieldMeta.getName());

      if (value == null) {
        PropertyUtils.setProperty(data, fieldName, null);
        return;
      }

      LocalDateTime localDateTime = null;
      try {
        localDateTime = dateTimeFormatter.parseLocalDateTime(value);
      } catch (IllegalArgumentException e) {
        LOGGER.debug("value format not valid", ExceptionUtils.getStackTrace(e));
      }
      PropertyUtils.setProperty(data, fieldName, localDateTime);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookProcessException(e);
    }
  }
}
