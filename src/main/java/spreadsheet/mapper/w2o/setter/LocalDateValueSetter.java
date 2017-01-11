package spreadsheet.mapper.w2o.setter;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.utils.FieldUtils;
import spreadsheet.mapper.w2o.processor.WorkbookProcessException;

/**
 * local date field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValueSetter<T> extends FieldValueSetterAdapter<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateValueSetter.class);

  private String pattern;

  public LocalDateValueSetter(String pattern, String matchField) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public void set(T object, Cell cell, FieldMeta fieldMeta) {
    try {
      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);
      String value = cell.getValue();
      String fieldName = FieldUtils.detectRealFieldName(fieldMeta);

      if (value == null) {
        PropertyUtils.setProperty(object, fieldName, null);
        return;
      }

      LocalDate localDate = null;
      try {
        localDate = dateTimeFormatter.parseLocalDate(value);
      } catch (IllegalArgumentException e) {
        LOGGER.debug("{} format not valid", value);
      }

      PropertyUtils.setProperty(object, fieldName, localDate);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookProcessException(e);
    }
  }
}
