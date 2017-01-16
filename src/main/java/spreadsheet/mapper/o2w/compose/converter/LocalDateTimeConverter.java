package spreadsheet.mapper.o2w.compose.converter;

import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.o2w.compose.WorkbookComposeException;
import spreadsheet.mapper.utils.FieldUtils;

/**
 * local date time text value with supplied pattern converter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeConverter<T> extends FieldConverterAdapter<T, LocalDateTimeConverter<T>> {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateTimeConverter.class);

  private String pattern;

  public LocalDateTimeConverter<T> pattern(String pattern) {
    this.pattern = pattern;
    return getThis();
  }

  @Override
  protected LocalDateTimeConverter<T> getThis() {
    return this;
  }

  @Override
  public String getStringValue(T object, Row row, FieldMeta fieldMeta) {

    try {
      Object value = PropertyUtils.getProperty(object, FieldUtils.detectRealFieldName(fieldMeta));

      if (!(value instanceof LocalDateTime)) {
        return null;
      }
      return ((LocalDateTime) value).toString(pattern);

    } catch (NestedNullException e) {
      LOGGER.debug("{} is null", fieldMeta.getName());
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }
}
